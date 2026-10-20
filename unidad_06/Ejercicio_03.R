#Ejercicio 3
??sleep75
#Función de Regresión Poblacional:
#sleep = BO + B1*totwrk + B2*educ + B3*age + B4*age^2 + B5*yngkid + u
#i) Estimar el modelo por separado entre hombres y mujeres y ver si hay diferencias importantes entre las ecuaciones estimadas
attach(sleep75)
str(sleep75)
#Para hombres:
modelo_1=lm(sleep~totwrk+educ+age+agesq+yngkid, data = sleep75 [sleep75$male==1,])
summary(modelo_1)
#Para mujeres
modelo_2=lm(sleep~totwrk+educ+age+agesq+yngkid, data = sleep75 [sleep75$male==0,])
summary(modelo_2)
#Comparamos los modelos
?stargazer
library(stargazer)
stargazer(modelo_1, modelo_2, type="text", title="Results", align=TRUE)
#Por lo tanto: Tomando únicamente las variables estadísticamente significativas,
#el intercepto que indica cuanto se duerme sin horas de trabajo, educación, años e hijos menores a 3 años
#es menor para los hombres que para las mujeres en alrededor de 600 minutos a la semana, y disminuye mas el sueño de los hombres ante un aumento de las horas de trabajo, en alrededor de 0,05 minutos semanales
#en relacion con la disminucion de minutos de sueño de las mujeres
#ii)
#modelo no restringido
modelo_no_restringido=lm(sleep~male+totwrk+(totwrk*male)+educ+(educ*male)+age+(age*male)+agesq+(agesq*male)+yngkid+(yngkid*male), data = sleep75)
summary(modelo_no_restringido)
#Estimamos la suma de cuadrados de los residuos del modelo anterior
SRCnR=sum(residuals(modelo_no_restringido)^2)
#modelo restringido
modelo_restringido=lm(sleep~totwrk+educ+age+agesq+yngkid, data = sleep75)
summary(modelo_restringido)
#Estimamos la suma de cuadrados de los residuos del modelo anterior
SRCR=sum(residuals(modelo_restringido)^2)
k=length(coefficients(modelo_restringido))
k
n1n2k= sum(male==1) + sum(male==0) - 2*k
Fc=((SRCR- SRCnR))*n1n2k /((SRCnR)*k)
?pf
pvFC=pf (Fc, k, n1n2k, lower.tail=FALSE)
pvFC
#Los gl pertinentes de esta prueba son 6 en el numerador y 694 en el denominador
#A un nivel de significancia del 5% se puede rechazar la hipótesis nula de que los interceptos y las pendientes son iguales para ambos sexos.
#iii) Ahora tomamos como que male y female tienen intereptos distintos, pero las interacciones conjuntas de male, son significativas?
#Primero el modelo restringido
modelo_4_R=lm(sleep~male+totwrk+educ+age+agesq+yngkid, data = sleep75)
summary(modelo_4_R)
#Su SRC
SRCR_2=sum(residuals(modelo_4_R)^2)
#Ahora el modelo no restringido
modelo_4_nR=lm(sleep~male+totwrk+(totwrk*male)+educ+(educ*male)+age+(age*male)+agesq+(agesq*male)+yngkid+(yngkid*male), data = sleep75)
summary(modelo_4_nR)
#Su SRCnR
SRCnR_2=sum(residuals(modelo_4_nR)^2)
q = length(coef(modelo_4_nR)) - length(coef(modelo_4_R))
gl_nr = df.residual(modelo_4_nR)
Fc=((SRCR_2-SRCnR_2)/q)/(SRCnR_2/gl_nr)
pvFC= pf(Fc,q,gl_nr,lower.tail = FALSE)
pvFC
anova(modelo_4_R,modelo_4_nR)
#A un nivel de significancia del 5% no hay evidencia estadisticamente significativa para rechazar la Hipótesis de que las interacciones entre la variable hombre y el resto de variables sean iguales a 0.
#iv) Concluímos que el modelo final es de la forma:
#sleep=B0+B1*male+B2*totwrk+B3*educ+B4*age+B5*agesq+B6*yngkid+u