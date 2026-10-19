#Ejercicio 1
#a) Probar distintos modelos
#1)
find("lm")
??wage2
library(stats)
library(wooldridge)
modelo_1=lm(wage ~ black, data = wage2)
summary(modelo_1)
plot(wage2$wage,wage2$black)
#2)
modelo_2=lm(wage~educ+exper+black,
            data = wage2 )
summary(modelo_2)
plot(wage2$wage,wage2$exper)
#3)
modelo_3=lm(lwage~educ+exper+black,
            data=wage2)
summary(modelo_3)
#4)
modelo_4=lm(lwage~educ+exper+black+married,
            data=wage2)
summary(modelo_4)
#b) Ver si la educacion depede de la raza sobre el modelo 3
modelo_3_revisado=lm(lwage~educ+exper+black+educ*black,
                      data=wage2)
summary(modelo_3_revisado)                     
#Al nivel de significación del 5%, no hay evidencia estadísticamente significativa para afirmar que el rendimiento salarial de la educación sea diferente entre los grupos
#c)Creamos las dummies
wage2$casadosnegros=ifelse(wage2$black=="1"&wage2$married=="1",1,
                           ifelse(wage2$black==""&wage2$married=="",NA,0))
wage2$casadosnegros
wage2$nocasadosnonegros=ifelse(wage2$black=="0"&wage2$married=="0",1,
                             ifelse(wage2$black==""&wage2$married=="",NA,0))
wage2$nocasadosnegros=ifelse(wage2$black=="1"&wage2$married=="0",1,
                             ifelse(wage2$black==""&wage2$married=="",NA,0))
modelo_4_mejorado=lm(lwage~educ+exper+casadosnegros+nocasadosnonegros+nocasadosnegros,
                     data=wage2)

summary(modelo_4_mejorado)
#B1:Ante un aumento de 1 año de experiencia, el salario mensual estimado en promedio aumenta un 7,19%, ceteris paribus el resto de variables
#B2:Ante un aumento de 1 año de educación, el salario mensual estimado en promedio aumenta un 1,79%, ceterisparibus el resto de variables
#B3:Si se esta casado y negro, el salario mensual estimado en promedio disminuye un 18,59% respecto a los no negros casados, ceteris paribus el resto de variables, ES ESTADÍSTICAMENTE SIGNIFICATIVA
#B4:Si se esta soltero y no se es negro, el salario mensual estimado en promedio disminuye un 17,21% respecto a los no negros casados, ceteris paribus el resto de variables, ES ESTADISTICAMENTE SIGNIFICATIVA
#B5:Si se esta soltero y se es negro, el salario mensual estimado en promedio disminuye un 34,98% respecto a los no negros casados, ceteris paribus el resto de vaiables, ES ESTADÍSTICAMENTE SIGNIFICATIVA
