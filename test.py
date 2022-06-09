import pygtide as pgt

pt = pgt.pygtide()
args = (-20.82071, -70.15288, 830.0, '1935-01-01', 6, 600)
pt.predict(*args, statazimut=90, tidalcompo=8)

data = pt.results()

print(data)