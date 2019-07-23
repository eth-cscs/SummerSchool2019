import numpy as np
import tensorflow as tf
import horovod.tensorflow as hvd


hvd.init()

# Create a linear function with noise as our data
nsamples = 1000
ref_slope = 2.0
ref_offset = 0.0
noise = np.random.random((nsamples, 1)) - 0.5
x_train = np.random.random((nsamples, 1)) - 0.5
y_train = ref_slope * x_train + ref_offset + noise

# Input pipeline
dataset = tf.data.Dataset.from_tensor_slices((x_train.astype(np.float32),
                                              y_train.astype(np.float32)))
dataset = dataset.shard(hvd.size(), hvd.rank())
dataset = dataset.batch(500)
dataset = dataset.repeat(100)
iterator = dataset.make_one_shot_iterator()
next_item = iterator.get_next()

# Define the model
slope = tf.Variable(np.random.randn())
offset = tf.Variable(np.random.randn())

x, y = next_item  # The model is the continuation of the pipeline

y_hat = slope * x + offset

loss = tf.losses.mean_squared_error(y_hat, y)

opt = tf.train.GradientDescentOptimizer(.5)
train = hvd.DistributedOptimizer(opt).minimize(loss)


hooks = [hvd.BroadcastGlobalVariablesHook(0)]

history = []

with tf.train.MonitoredTrainingSession(hooks=hooks) as sess:
    # Initialization of the variables `slope` and `offset`
    # is done automatically by tf.train.MonitoredTrainingSession
    print('rank', hvd.rank(),
          'inital slope   = %12.6f\n       initial offset = %12.6f' %
          sess.run((slope, offset)))
    while not sess.should_stop():
        _, loss_val, m, n = sess.run((train, loss, slope, offset))
        history.append([sess.run(slope), sess.run(offset), loss_val])

# save data for visualization
np.save('slope_hist_%s' % hvd.rank(), np.array(history)[:, 0])
np.save('offset_hist_%s' % hvd.rank(), np.array(history)[:, 1])
if hvd.rank() == 0:
    np.save('x_train', x_train)
    np.save('y_train', y_train)
