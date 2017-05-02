# -*- coding: utf-8 -*-
"""
Created on Wed Apr 12 00:47:36 2017

@author: XMKZ
"""

import numpy as np
import pandas as pd
np.random.seed(1337) 
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.optimizers import RMSprop
from keras.utils import np_utils
data = pd.read_csv('D:/大四下/data science/final/sub.csv', header=0)
data = np.array(data)
X_train = data[0:30000,0:253]
X_test = data[30001:40000,0:253]
y_train = data[0:30000,254]
y_test = data[30001:40000,254]
y_train = np_utils.to_categorical(y_train, num_classes=3)
y_test = np_utils.to_categorical(y_test, num_classes=3)



model = Sequential([
    Dense(64,input_dim=254),
    Activation('relu'),
    Dense(10),
    Activation('relu'),
    Dense(3),
    Activation('softmax'),
])

rmsprop = RMSprop(lr=0.001)

model.compile(optimizer=rmsprop,
              loss='sparse_categorical_crossentropy',
              metrics=['sparse_categorical_accuracy'])

print('Training ------------')
# Another way to train the model
model.fit(X_train, y_train, epochs=20, verbose=2, validation_split=0.2, shuffle=True, batch_size=1000)

print('\nTesting ------------')
# Evaluate the model with the metrics we defined earlier
loss, accuracy = model.evaluate(X_test, y_test)

print('test loss: ', loss)
print('test accuracy: ', accuracy)

