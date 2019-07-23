import os
import numpy as np
import tensorflow as tf
# import horovod to work with tensorflow.keras
from tensorflow import keras


def decode(serialized_example):
    features = tf.parse_single_example(
        serialized_example,
        features={
            'image/encoded': tf.FixedLenFeature([], tf.string),
            'image/class/label': tf.FixedLenFeature([], tf.int64),
        })
    image = tf.image.decode_jpeg(features['image/encoded'], channels=3)
    image = tf.image.resize_images(image, (224, 224))
    label = tf.cast(features['image/class/label'], tf.int64)
    label = tf.one_hot(label, 1001)
    return image, label


def train_input():
    data_dir = '/scratch/snx3000/stud32/imagenet/'
    list_of_files = [os.path.join(data_dir, f) for f in os.listdir(data_dir)]
    dataset = tf.data.Dataset.list_files(list_of_files)
    dataset = dataset.interleave(tf.data.TFRecordDataset,
                                 cycle_length=120,
                                 block_length=1,
                                 num_parallel_calls=12)
    dataset = dataset.map(decode, num_parallel_calls=12)
    # shard the dataset
    dataset = dataset.batch(64)
    return dataset


model = keras.applications.ResNet50(weights=None,
                                 input_shape=(224, 224, 3),
                                 classes=1001)

# initialize horovod

optimizer = keras.optimizers.SGD(lr=0.0001, momentum=0.9)

# wrap optimizer in horovod's optimizer

# Create a callback to sync the intial state


# save the checkpoints of rank 0 only
# callbacks.append(keras.callbacks.ModelCheckpoint(
#     os.path.join(os.environ['SCRATCH'], './checkpoint-{epoch}.h5')))

model.compile(optimizer=optimizer,
              loss='categorical_crossentropy',
              metrics=['accuracy'])

fit = model.fit(train_input(),
                epochs=2,
                steps_per_epoch=100
                )
