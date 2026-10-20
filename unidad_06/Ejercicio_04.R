#Ejercicio 4
#i)
?loanapp
#Consideramos la FRP:
#approve=B0+B1+white+other factors
#Si se han controlado los factores adecuados y hay discriminación contra las minorías, el signo de B1 será positivo, es decir la tasa de aprobación de los prestamos hipotecarios será mayor si se es blanco.
#ii)
modelo_1=lm(approve~white, data = loanapp)
summary(modelo_1)
# Los solicitantes blancos presentan una probabilidad estimada de aprobación 20,06 puntos porcentuales mayor que los no blancos.
# La diferencia es estadísticamente significativa al 5%, dado que el p-valor es menor que 0,05.
# La magnitud es considerable: la tasa estimada de aprobación es del 70,78% para los no blancos y del 90,84% para los blancos.
#iii) Agregamos el resto de variables que pide:
modelo_2=lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,data =loanapp)
summary(modelo_2)
#Al agregar el resto de variables el coeficiente B1 disminuye a 0,129, lo que indica que las personas blancas en promedio obtienen un 12,9% mas de aprobaciones de créditos hipotecarios que las no blancas.
#El coeficiente sigue siendo estadísticamente significativo a un nivel de significancia del 5% ya que el p-valor<0,05.
#Por lo tanto se puede sostener la discriminación contra los no blancos a la hora de asignar préstamos hipotecarios.
#iv)
modelo_3=lm(approve~white*obrat+hrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,data =loanapp)
summary(modelo_3)
#Este temino de interacción entre los individuos blancos y otras obligaciones como porcentaje del ingreso es estadísticamente significativo a un nivel del 5%.
#v) Para calcular el efecrto de ser blanco sobre la probabilidad de aprobacion tomamos:
modelo_4 = lm(approve~white+hrat+obrat+I((obrat-32)*white)+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,data =loanapp)
summary(modelo_4)
# Efecto de ser blanco cuando obrat = 32
coef(modelo_4)["white"]

# Intervalo de confianza del 95% para ese efecto
confint(modelo_4, "white", level = 0.95)
