import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import tensorflow as tf
# hvd: import horovod


# hvd: initialize horovod

tf.enable_eager_execution()

tf.VERSION

# Create a linear function with noise as our data
nsamples = 1000
ref_slope = 2.0
ref_offset = 0.0
noise = np.random.random((nsamples, 1)) - 0.5    # -0.5 to center the noise
x_train = np.random.random((nsamples, 1)) - 0.5  # -0.5 to center x around 0
y_train = ref_slope * x_train + ref_offset + noise

dataset = tf.data.Dataset.from_tensor_slices((x_train.astype(np.float32),
                                              y_train.astype(np.float32)))
# hvd: split the dataset over ranks
dataset = dataset.batch(500)
dataset = dataset.repeat(100)

opt = tf.train.GradientDescentOptimizer(learning_rate=0.5)

history = []
slope = tf.Variable(np.random.randn(), name='slope')
offset = tf.Variable(np.random.randn(), name='offset')

# hvd: broadcast the initial values of the variables from
# rank 0 to the orther ranks


print(# 'rank', hvd.rank(),
      'inital slope   = %12.6f\n       initial offset = %12.6f' %
      (slope.numpy(), offset.numpy()))

for xtr, ytr in dataset:
    with tf.GradientTape() as tape:
        yhat = slope * xtr + offset
        loss = tf.losses.mean_squared_error(yhat, ytr)
        
    # hvd: replace tensorflows' GradientTape for Horovod's
    # so that the gradients from all ranks are averaged

    grads = tape.gradient(loss, [slope, offset])
    opt.apply_gradients(zip(grads, [slope, offset]),
                        global_step=tf.train.get_or_create_global_step())

    
    history.append([slope.numpy(), offset.numpy(), loss.numpy()])
    # hvd: print also the rank number (like loss = 0.083064 (rank-x))
    # tf.print('loss = %f' % loss)

# hvd: uncomment the following to save the history for plotting
# hvd: fill in the right function to get the ranks
# np.save('slope_hist_%s' % <hvd rank>, np.array(history)[:, 0])
# np.save('offset_hist_%s' % <hvd rank>, np.array(history)[:, 1])

# hvd: save x_train and y_train but only in the first rank
# to avoid both ranks writing the same file
# np.save('x_train', x_train)
# np.save('y_train', y_train)
