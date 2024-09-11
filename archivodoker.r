library(dplyr)
library(RODBC)
library(lpSolve)
library(jsonlite)

##### Variables ####

##### End dariables ####

dbhandle <- odbcDriverConnect("driver={ODBC Driver 17 for SQL Server};
                                server=DESKTOP-JLOBEF0\\SQLEXPRESS;
                                database=BD_OPTIMIZACION_CENTINELA;
                                Trusted_Connection=yes;")

# Version 1.11

# Leer parámetros desde archivo JSON
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]

data <- fromJSON(input_file)

# Extraer los valores de idSim y idSubSim
idSim <- data$idSim
idSubSim <- data$idSubSim


simulacion <- function(idSim,idSubSim){
  
  fechaInicSim <- Sys.time() - 18000
  
  #### ----- 1. Importación BD ----- ####
  
  
  # dbhandle <- odbcDriverConnect(paste("driver={SQL Server};",
  #                                     "server=",server,
  #                                     ";database=",database,
  #                                     ";uid=",uid,
  #                                     ";pwd=",pwd,";"))
  
  if (idSubSim==1) {
    simDetalle <- sqlQuery(dbhandle, paste('[ModR].[Usp_Obt_SimulacionInput] ',idSim))
  } else if (idSubSim==2) {
    simDetalle <- sqlQuery(dbhandle, paste('[ModR].[Usp_Obt_SimulacionInputSiguiente] ',idSim))
  }
  
  #### END Importación BD ####
  
  #### ----- 2. Funciones ----- ####
  
  SimulacionDetalle <- function(idSimulacionDetalle){
    
    #### ----- 2.1. Algoritmo de optimización ----- ####
    
    # Función del algoritmo de optimización
    Asignacion <- function(embarcacion,planta,permisoInput){
      
      nEmb <- nrow(embarcacion)
      nPlan <- nrow(planta)
      coef <- permisoInput$IngresoVariable #Matriz de coeficientes de la función objetivo
      rhs <- c(rep(1,nEmb),planta$TMRestante) #Lado derecho de las restricciones
      
      #RESTRICCIONES
      
      #Creación de matriz de restricciones
      Amatrix <- matrix(0,nEmb+nPlan,nrow(permisoInput))
      # Un embarcación es asignada a una sola planta y zona de pesca
      for (i in 1:nrow(permisoInput)) {
        Amatrix[which(embarcacion$IdEmbarcacion==permisoInput$IdEmbarcacion[i]),i] <- 1
      }
      # No enviar más de la capacidad disponible de planta
      for (i in 1:nrow(permisoInput)) {
        Amatrix[which(planta$IdPlanta==permisoInput$IdPlanta[i])+nEmb,i] <- 
          permisoInput$TMaProcesar[i]
      }
      signos <- c(rep("==", nEmb),rep("<=", nPlan)) # Signo de las restricciones
      
      solAss01 <- lp(direction = "max", objective.in = coef, const.mat = Amatrix,
                     const.dir = signos, const.rhs = rhs, transpose.constraints = TRUE,
                     all.int = TRUE)
      #Matriz de la solución
      solucion <- permisoInput[which(solAss01$solution==1),]
      return(solucion)
    }
    
    
    #### END Algoritmo de optimización ####
    
    #### ----- 2.2. Importar de BD ----- ####
    
    # idSimulacionDetalle <- 8200
    
    if (idSubSim==1) {
      simulInput <- sqlQuery(dbhandle, 
                             paste('SET NOCOUNT ON exec [ModR].[Usp_Obt_SimulacionInputDetalle] ',
                                   idSimulacionDetalle))
      embarcacion <- sqlQuery(dbhandle, paste('[ModR].[Usp_Obt_SimulacionInputEmbarcacion] ',
                                              idSimulacionDetalle))
      MZP <- sqlQuery(dbhandle, paste('[ModR].[Usp_Obt_SimulacionMicroZonaPesca] ',idSimulacionDetalle))
      planta <- sqlQuery(dbhandle, paste('[ModR].[Usp_Obt_SimulacionPlanta] ',idSimulacionDetalle))
      restriccion <- sqlQuery(dbhandle, paste('[ModR].[Usp_Obt_SimulacionRestriccion] ',
                                              idSimulacionDetalle))
    } else if (idSubSim==2) {
      simulInput <- sqlQuery(dbhandle, 
                             paste('SET NOCOUNT ON exec [ModR].[Usp_Obt_SimulacionInputSiguienteDetalle] ',
                                   idSimulacionDetalle))
      embarcacion <- sqlQuery(dbhandle, paste('[ModR].[Usp_Obt_SimulacionInputSiguienteEmbarcacion] ',
                                              idSimulacionDetalle))
      MZP <- sqlQuery(dbhandle, paste('[ModR].[Usp_Obt_SimulacionMicroZonaPescaSiguiente] ',idSimulacionDetalle))
      planta <- sqlQuery(dbhandle, paste('[ModR].[Usp_Obt_SimulacionPlantaSiguiente] ',idSimulacionDetalle))
      restriccion <- sqlQuery(dbhandle, paste('[ModR].[Usp_Obt_SimulacionRestriccionSiguiente] ',
                                              idSimulacionDetalle))
    }
    
    # Para realizar la prueba de solución
    simulInputCombinatorio <- distinct(simulInput,IdEmbarcacion,IdMicroZonaPesca,IdPlanta)
    permiso <- anti_join(simulInputCombinatorio,restriccion)
    
    # constantes
    margenPlanta <- 0.2
    
    
    #### end importar BD ####
    
    #### ----- 2.3. Conversión data input ----- ####
    
    nEmb <- nrow(embarcacion)
    nPlan <- nrow(planta)
    nZP <- nrow(MZP)
    
    planta$TMRestante <- planta$TMRestante
    
    # Para realizar la prueba de solución
    embarcacionPlanta <- distinct(simulInput,IdEmbarcacion,IdPlanta)
    permisoPlanta <- distinct(permiso,IdEmbarcacion,IdPlanta)
    restriccionPlanta <- anti_join(embarcacionPlanta,permisoPlanta)
    permisoInput <- left_join(permiso,simulInput)
    
    #### END Conversión data input ####
    
    #### ----- 2.4. Simular por embarcación ----- ####
    
    permisoInput <- left_join(permisoInput,embarcacion,"IdEmbarcacion")
    permisoInput <- left_join(permisoInput,planta,"IdPlanta")
    
    for (i in 1:nEmb) {
      
      if (nrow(planta)==0 | nrow(embarcacion)==0 | nrow(permisoInput)==0) {
        break
      }
      
      plantaPrueba <- planta
      plantaPrueba$TMRestante <- planta$TMRestante*20
      
      borrado <- which(permisoInput$TMRestante<permisoInput$TMaProcesar)
      
      if ( length(borrado)>0 ) {
        permisoInput <- permisoInput[-borrado,]
      }
      
      if (nrow(planta)==0 | nrow(embarcacion)==0 | nrow(permisoInput)==0) {
        break
      }
      
      for (j in sort(unique(permisoInput$IdPlanta)) ) {
        if (j==sort(unique(permisoInput$IdPlanta))[1]) {
          plantaQueda <- which(planta$IdPlanta==j)
        } else {
          plantaQueda <- c(plantaQueda,which(planta$IdPlanta==j))
        }
      }
      planta <- planta[plantaQueda,]
      
      for (j in sort(unique(permisoInput$IdEmbarcacion)) ) {
        if (j==sort(unique(permisoInput$IdEmbarcacion))[1]) {
          embarcacionQueda <- which(embarcacion$IdEmbarcacion==j)
        } else {
          embarcacionQueda <- c(embarcacionQueda,which(embarcacion$IdEmbarcacion==j))
        }
      }
      
      embarcacion <- embarcacion[embarcacionQueda,]
      
      if (nrow(planta)==0 | nrow(embarcacion)==0 | nrow(permisoInput)==0) {
        break
      }
      
      # SIMULACION
      solParcial <- Asignacion(embarcacion,plantaPrueba,permisoInput)
      
      # Determ. indices de embarcaciones por prioridades (1.Terceros 2.PropiosNoFríos 3.PropiosFrios)
      
      if ( length(which(embarcacion$FlagAsignacionFija==1))>0 ) { # Prioridad embarcaciones fijadas
        if ( length(which(embarcacion$FlagPropio==0 & 
                          embarcacion$FlagAsignacionFija==1))>0 ) { # Prioridad embarcaciones terceras
          indEmbTipo <- which(embarcacion$FlagPropio==0 & embarcacion$FlagAsignacionFija==1)
        } else {
          if (length(which(embarcacion$FlagPropio==1 & embarcacion$FlagconRSW==0 & 
                           embarcacion$FlagAsignacionFija==1))>0) {# Prior. emb propias no frías
            indEmbTipo <- which((embarcacion$FlagPropio==1) & (embarcacion$FlagconRSW==0) & 
                                  (embarcacion$FlagAsignacionFija==1))
          } else { # Prior. emb propias frías
            indEmbTipo <- which(embarcacion$FlagPropio==1 & embarcacion$FlagconRSW==1 & 
                                  embarcacion$FlagAsignacionFija==1)
          }
        }
      } else {# Embarcaciones no fijadas
        if ( length(which(embarcacion$FlagPropio==0 & 
                          embarcacion$FlagAsignacionFija==0))>0 ) { # Prioridad embarcaciones terceras
          indEmbTipo <- which(embarcacion$FlagPropio==0 & embarcacion$FlagAsignacionFija==0)
        } else {
          if (length(which(embarcacion$FlagPropio==1 & embarcacion$FlagconRSW==0 & 
                           embarcacion$FlagAsignacionFija==0))>0) {# Prior. emb propias no frías
            indEmbTipo <- which((embarcacion$FlagPropio==1) & (embarcacion$FlagconRSW==0) & 
                                  (embarcacion$FlagAsignacionFija==0))
          } else { # Prior. emb propias frías
            indEmbTipo <- which(embarcacion$FlagPropio==1 & embarcacion$FlagconRSW==1 & 
                                  embarcacion$FlagAsignacionFija==0)
          }
        }
      }
      
      # indMinFHoraArribo <- which.min(solParcial$FechaHoraArriboPlanta[indEmbTipo])
      indMinFHoraArribo <- which.max(solParcial$IngresoVariable[indEmbTipo])
      indMinFHoraArribo <- indEmbTipo[indMinFHoraArribo]
      
      idEmbMinFHArribo <- embarcacion$IdEmbarcacion[indMinFHoraArribo]
      
      if (i==1) {
        solFinal <- solParcial[indMinFHoraArribo,]
      } else {
        solFinal[nrow(solFinal)+1,] <- solParcial[indMinFHoraArribo,]
      }
      
      planta$TMRestante[planta$IdPlanta==solParcial$IdPlanta[indMinFHoraArribo]] <- 
        planta$TMRestante[planta$IdPlanta==solParcial$IdPlanta[indMinFHoraArribo]] - 
        embarcacion$TMaProcesar[embarcacion$IdEmbarcacion==solParcial$IdEmbarcacion[indMinFHoraArribo]]
      
      permisoInput$TMRestante[permisoInput$IdPlanta==solParcial$IdPlanta[indMinFHoraArribo]] <- 
        planta$TMRestante[planta$IdPlanta==solParcial$IdPlanta[indMinFHoraArribo]]
      
      # Quitar embarcación con menor fecha hora de arribo
      permisoInput <- permisoInput[-which(permisoInput$IdEmbarcacion==idEmbMinFHArribo),]
      embarcacion <- embarcacion[-which(embarcacion$IdEmbarcacion==idEmbMinFHArribo),]
      
    }
    
    #### END Simular por embarcación ####
    
    #### ----- 2.5. Crear matriz output ----- ####
    
    solFinal <- solFinal[,-(5:ncol(solFinal))]
    solucion <- solFinal
    
    #### END Crear matriz output ####
    
    return(solucion)
  }
  
  #### END Funciones ####
  
  #### ----- 3. Simular variación por repetición ----- ####
  
  sumaIV <- 0
  for (i in 1:nrow(simDetalle)) {
    
    solucionParcial <- SimulacionDetalle(simDetalle$IdSimulacionInput[i])
    sumaIVParcial <- sum(solucionParcial$IngresoVariable)
    
    if (sumaIVParcial>sumaIV) {
      sumaIV <- sumaIVParcial
      idSolSimDet <- i
      simulacionOutputDetalle <- solucionParcial
    }
    
    if (i==1) {
      simulacionOutput <- data.frame("IdSimulacionInput"=simDetalle$IdSimulacionInput[i],
                                     "IngresoVariableTotal"=sumaIVParcial,
                                     "FlagSeleccionado"=0)
      
    } else {
      simulacionOutput[i,] <- data.frame(simDetalle$IdSimulacionInput[i],sumaIVParcial,0)
    }
    
  }
  
  simulacionOutput$FlagSeleccionado[which.max(simulacionOutput$IngresoVariableTotal)] <- 1
  
  simulacionOutputDetalle <- 
    data.frame("IdSimulacionInput"=simulacionOutput$IdSimulacionInput[which.max(simulacionOutput$IngresoVariableTotal)],
               simulacionOutputDetalle)
  
  #### END Simular variación por repetición ####
  
  #### ----- 4. Guardar en BD ----- ####

  #print(simulacionOutput)
  #print(simulacionOutputDetalle)

  fechaFinSim <- Sys.time() - 18000

  #print(format(fechaInicSim, "%Y-%m-%d %H:%M:%S"))
  #print(format(fechaFinSim, "%Y-%m-%d %H:%M:%S"))

  if (idSubSim==1) {
    # Guardar en la BD
    ###
    #print(simulacionOutputDetalle)
    ###
    sqlUpdate(dbhandle,simulacionOutput,tablename = "Opt.SimulacionInput", index = "IdSimulacionInput")
    sqlSave(dbhandle,simulacionOutputDetalle,tablename = "Opt.SimulacionOutputDetalle",rownames = F, append = TRUE)
    # SPs distribuir
    sqlQuery(dbhandle, paste('[Opt].[Usp_DistribuirSimulacionOutputRequerimientoSolicitado] ',idSim))
    # Actulizar tabla simulación
    simulacion <- data.frame("IdSimulacion"=idSim,
                             "FechaHoraInicioR"=format(fechaInicSim, "%Y-%m-%d %H:%M:%S"),
                             "FechaHoraFinR"=format(fechaFinSim, "%Y-%m-%d %H:%M:%S"))
    sqlUpdate(dbhandle,simulacion,tablename = "Opt.Simulacion", index = "IdSimulacion")
    
  } else if (idSubSim==2) {
    # Guardar en la BD
    ###
    #print(simulacionOutputDetalle)
    ###
    sqlUpdate(dbhandle,simulacionOutput,tablename = "Opt.SimulacionInputSiguiente", index = "IdSimulacionInput")
    sqlSave(dbhandle,simulacionOutputDetalle,tablename = "Opt.SimulacionOutputSiguienteDetalle",rownames = F, append = TRUE)
    # SPs distribuir
    sqlQuery(dbhandle, paste('[Opt].[Usp_DistribuirSimulacionOutputRequerimientoSiguiente] ',idSim))
    # Actulizar tabla simulación
    simulacion <- data.frame("IdSimulacion"=idSim,
                             "FechaHoraInicioRSiguiente"=fechaInicSim,
                             "FechaHoraFinRSiguiente"=fechaFinSim)
    sqlUpdate(dbhandle,simulacion,tablename = "Opt.Simulacion", index = "IdSimulacion")
  }
  
  #### END Guardar en BD  #### 
  print(simulacionOutputDetalle)
}

# Correr simulación 1
#data <- simulacion(idSim = 1485,idSubSim = 1)


# SPs preparar simulación siguiente
#sqlQuery(dbhandle, paste('[Opt].[Usp_Pre_Ejecutar_SimulacionSiguiente] ',idSim=1478))

# Correr simulación 2
#simulacion(idSim = 1478,idSubSim = 2)

#rm(list = ls())

