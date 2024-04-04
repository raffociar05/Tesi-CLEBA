#importazione librerie
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from keras.models import Sequential
from keras.layers import LSTM, Dropout, Dense
from keras.callbacks import EarlyStopping
from keras.utils import plot_model

#dataframe originario
df_original = pd.read_excel('df_temp_media_bis.xlsx')
df_original['Data'] = pd.to_datetime(df_original['Data'])
df_original.set_index('Data', inplace=True)

# Creazione della serie originaria
plt.figure(figsize=(12, 7))
plt.plot(df_original['Gradi'], color='black')
plt.xlabel('Anni')
plt.ylabel('Temperatura (°C)')
plt.title('Andamento della Temperatura Media Italiana')
plt.legend()
plt.grid(True)
plt.show()

# importazione dei dati su cui lavorare
df = pd.read_excel('df_temp_media_bis.xlsx')

#Preprocessing

#Conversione della variabile "Data" nel formato datetime e impostazione come indice
df['Data'] = pd.to_datetime(df['Data'])
df.set_index('Data', inplace=True)

#Stadardizzazione della variabile "Gradi"
scaler = StandardScaler()
df['Gradi'] = scaler.fit_transform(df[['Gradi']])

#Suddivisione del df in training e test set
train_size = int(len(df) * 0.8)
test_size = len(df) - train_size
train, test = df.iloc[0:train_size], df.iloc[train_size:len(df)]

# Creazione del grafico train/test
plt.figure(figsize=(12, 7))
plt.plot(train['Gradi'], color='black', label='Training Set')
plt.plot(test['Gradi'], color='red', label='Test Set')
plt.xlabel('Anni')
plt.ylabel('Temperatura (°C)')
plt.title('Andamento delle Temperatura media italiana')
plt.legend()
plt.grid(True)
plt.show()

#Creazione dataset per la rete LSTM
def create_dataset(X, time_steps=1):
    Xs, ys = [], []
    for i in range(len(X) - time_steps):
        v = X.iloc[i:(i + time_steps)].values
        Xs.append(v)
        ys.append(X.iloc[i + time_steps])
    return np.array(Xs), np.array(ys)

time_steps = 1
X_train, y_train = create_dataset(train, time_steps)
X_test, y_test = create_dataset(test, time_steps)

#Costruzione e compilazione della rete neurale LSTM
model = Sequential()
model.add(LSTM(64, activation='tanh', return_sequences=True, input_shape=(X_train.shape[1], X_train.shape[2])))
model.add(Dropout(0.2))
model.add(LSTM(32, activation='tanh', return_sequences=True))
model.add(Dropout(0.2))
model.add(LSTM(16, activation='tanh', return_sequences=False))
model.add(Dense(1))
model.compile(optimizer='adam', loss='mean_squared_error')

#Early Stopping
early_stop = EarlyStopping(monitor='val_loss', patience=10)
#Addestramento del modello
history = model.fit(X_train, y_train,
                    epochs=100,
                    batch_size=32,
                    validation_split=0.1,
                    callbacks=[early_stop],
                    shuffle=False)

#salvataggio del modello LSTM
model.save('model_LSTM.keras')
model.summary()
plot_model(model)

#Grafico della funzione di loss (trian/validation)
plt.plot(history.history['loss'], label='Training Loss')
plt.plot(history.history['val_loss'], label='Validation Loss')
plt.xlabel('Epoch')
plt.ylabel('Loss')
plt.legend()
plt.show()

#Previsioni
predictions = model.predict(X_test)
predictions = scaler.inverse_transform(predictions)
y_test = scaler.inverse_transform(y_test)

test

# Creazione di un array di date (19-10-2020 / 31-12-2023) per il test set
date = pd.date_range(start='2020-10-19', end='2023-12-31')

# Controllo lunghezza array tra y_test e predictions
if len(date) != len(y_test):
    date = date[:len(y_test)]

#Grafico della serie con valori predetti (fitted) e reali (test set)
plt.figure(figsize=(10, 6))
plt.plot(date, y_test, color='blue', label='Dati reali')
plt.plot(date, predictions, color='red', label='Dati predetti')
plt.legend(loc='upper right')
plt.title('Confronto tra dati reali e predetti')
plt.xlabel('Anni')
plt.ylabel('Gradi (°C)')
plt.grid(True)

plt.show()
#previsione gradi
predicted_grades = model.predict(X_test)

#inversione della normalizzazione per riportare i dati ai valori originari
predicted_grades_inverse = scaler.inverse_transform(predicted_grades)

predicted_grades_inverse_df = pd.DataFrame(predicted_grades_inverse)

predicted_grades_inverse_df.describe()
plt.plot(df_original['Gradi']) #serie originaria

#date future per la previsione
new_date = pd.date_range(start='2024-01-01', end='2027-03-13')

predicted_grades_inverse_df['Data'] = new_date
predicted_grades_inverse_df

#rinominazione della variabile "Gradi"
predicted_grades_inverse_df = predicted_grades_inverse_df.rename(columns={predicted_grades_inverse_df.columns[0]: 'Gradi'})

predicted_grades_inverse_df

#riodinamento colonne nel df
cols = list(predicted_grades_inverse_df.columns)
cols[0], cols[1] = cols[1], cols[0]
predicted_grades_inverse_df = predicted_grades_inverse_df.reindex(columns=cols)

predicted_grades_inverse_df
predicted_grades_inverse_df.describe()
df_original = pd.DataFrame(df_original)
df_original
#creazione df con valori originari + previsioni
df_new = pd.concat([df_original, predicted_grades_inverse_df], ignore_index=True)
df_new

# Grafico serie con dari originari + previsioni
plt.figure(figsize=(12, 7))
plt.plot(df_original['Data'], df_original['Gradi'], linestyle='-', color='gray', label='Dati originari')
plt.plot(predicted_grades_inverse_df['Data'], predicted_grades_inverse_df['Gradi'], linestyle='-', color='red', label='Previsioni future')
plt.xlabel('Anni')
plt.ylabel('Gradi (°C)')
plt.title('Temperatura media italiana')
plt.grid(True)
plt.xticks(rotation=45)
plt.legend() #legenda


plt.show()
