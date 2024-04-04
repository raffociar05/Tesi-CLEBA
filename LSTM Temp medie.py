#Importazione librerie

import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense
from tensorflow.keras.callbacks import EarlyStopping
import matplotlib.pyplot as plt

# Caricamento del dataframe
df = pd.read_excel("df_temp_media_bis.xlsx")

# Visualizzazione delle prime righe
print(df.head())

# Preprocessing dei dati

# Conversione della variabile "Data" in formato datetime
df['Data'] = pd.to_datetime(df['Data'])

'''
Quando si usa reshape(-1, 1), si chiede a numpy di rimodellare l'array
con 1 colonna e tante righe quante sono necessarie per contenere i dati.
Questa operazione darà come risultato un array 2D di forma (n, 1),
dove n è il numero di elementi dell'array originale.
'''

data = df['Gradi'].values.reshape(-1, 1)

# Normalizzazione dei dati tra i valori 0 e 1
scaler = MinMaxScaler(feature_range=(0, 1))
data_normalized = scaler.fit_transform(data)

# Creazione delle sequenze temporali
def create_sequences(data, seq_length):
    X, y = [], []
    for i in range(len(data) - seq_length):
        X.append(data[i:i+seq_length])
        y.append(data[i+seq_length])
    return np.array(X), np.array(y)

# Definizione della lunghezza della sequenza temporale
seq_length = 30

# Creazione delle sequenze temporali
X, y = create_sequences(data_normalized, seq_length)

# Divisione dei dati in set di addestramento e di test
train_size = int(len(X) * 0.7)
X_train, X_test = X[:train_size], X[train_size:]
y_train, y_test = y[:train_size], y[train_size:]

# Definizione del modello LSTM
model = Sequential()
model.add(LSTM(units=50, activation='relu', return_sequences=True,input_shape=(seq_length, 1)))
# Secondo layer LSTM
model.add(LSTM(units=50, return_sequences=True))
# Terzo layer LSTM
model.add(LSTM(units=50))
# Layer completamente connesso
model.add(Dense(units=1))

# Compilazione del modello
model.compile(optimizer='adam', loss='mean_squared_error')

# Addestramento del modello
early_stopping = EarlyStopping(monitor='val_loss', patience=5, restore_best_weights=True)
history = model.fit(X_train, y_train, epochs=100, batch_size=32, validation_split=0.2, callbacks=[early_stopping])



model.save('lstm_model.keras')

model.summary()

#!pip install graphviz
from keras.utils import plot_model
plot_model(model)

# Visualizzazione dell'andamento della loss durante l'addestramento
plt.plot(history.history['loss'], label='Training Loss')
plt.plot(history.history['val_loss'], label='Validation Loss')
plt.xlabel('Epochs')
plt.ylabel('Loss')
plt.legend()
plt.show()



# Valutazione del modello
train_loss = model.evaluate(X_train, y_train, verbose=0)
test_loss = model.evaluate(X_test, y_test, verbose=0)
print(f'Train Loss: {train_loss}')
print(f'Test Loss: {test_loss}')


'''
I risultati che si ottengono dall'addestramento della rete LSTM mostrano le perdite sul training set e sul test set.
La perdita, spesso chiamata errore di training o errore di test, è una misura della discrepanza tra i valori previsti dal 
modello e i valori reali nei dati di training e di test, rispettivamente.
Train Loss (Perdita sul training set): La perdita sul training set è la misura dell'errore del modello sui dati che sono 
stati utilizzati per addestrare il modello stesso. Una bassa perdita sul training set indica che il modello è stato in grado
di adattarsi bene ai dati di training e ha imparato a rappresentarli efficacemente.
Test Loss (Perdita sul test set): La perdita sul test set è la misura dell'errore del modello sui dati che non sono stati 
utilizzati durante l'addestramento, ma sono stati riservati appositamente per valutare le prestazioni del modello su dati non visti.
Una bassa perdita sul test set indica che il modello è in grado di generalizzare bene oltre ai dati di training e può fare
previsioni accurate su nuovi dati.
In questo caso, si ha una perdita leggermente maggiore sul test set rispetto al training set. 
Tuttavia, le differenze tra la perdita sul training set e quella sul test set sembrano essere relativamente piccole, il che 
potrebbe indicare che il modello sta generalizzando bene e non sta soffrendo di overfitting (cioè, non sta adattando troppo 
strettamente i dati di training).
In generale, questi risultati indicano che il modello ha imparato a rappresentare efficacemente i dati di training e a 
generalizzare bene su nuovi dati, il che è un segno positivo della sua capacità di fare previsioni accurate.
'''


# Previsione sui prossimi 3 anni
future_predictions = []
last_sequence = X[-1]
for _ in range(365 * 3):  # 3 anni
    prediction = model.predict(last_sequence.reshape(1, seq_length, 1))
    future_predictions.append(prediction[0, 0])
    last_sequence = np.roll(last_sequence, -1)
    last_sequence[-1] = prediction

# Denormalizzazione delle previsioni
future_predictions_denormalized = scaler.inverse_transform(np.array(future_predictions).reshape(-1, 1))



# Plot delle previsioni
plt.plot(df['Data'], df['Gradi'], label='Dati Storici')
plt.plot(pd.date_range(start='2024-01-01', periods=len(future_predictions_denormalized), freq='D'), future_predictions_denormalized, label='Previsioni Future', linestyle='--')
plt.xlabel('Data')
plt.ylabel('Temperatura (°C)')
plt.title('Previsione delle Temperature Future')
plt.legend()
plt.show()

print(future_predictions_denormalized)

future_predictions_denormalized_df = pd.DataFrame(future_predictions_denormalized)
future_predictions_denormalized_df.to_csv('future_predictions_denormalized.csv')

df['Data']
