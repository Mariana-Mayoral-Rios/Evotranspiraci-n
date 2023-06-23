#' Evotranspiración.
#'
#' La función final realiza el calculo de la evotranspiración de un cultivo.
#'
#' @param t (variable) dato perteneciente al cálculo de saturación de vapor.
#' @param tma (variable) dato pertenciente al cálculo de flujo de calor.
#' @param tmp (variable) dato pertenciente al cálculo de flujo de calor.
#' @param Z (variable) dato pertenciente al cálculo de la constante psicometrica.
#' @param uz (variable) dato pertenciente al cálculo de la velocidad del viento.
#' @param z (variable) dato pertenciente al cálculo de la velocidad del viento.
#' @param es (variable) dato pertenciente al cálculo del déficit de presión de vapor en la atmosfera.
#' @param ea (variable) dato pertenciente al cálculo del déficit de presión de vapor en la atmosfera.
#' @param Rn (variable) dato perteneciente a la formula principal.
#' @param Tm (variable) dato perteneciente a la formula principal.
#' @return Resultado de toda la formula de la evotranspiración.
#' @export
evotranspiración <- function(t, tma, tmp, Z, uz, z, es, ea, Rn, Tm){
  ### Cálculo de saturación de vapor.
  Sv <- (2503.6*((17.27)*(t))/(t+237.3))/((t+237.3)^2)

  ### Cálculo de flujo de calor.
  Fc <- (0.07*(tma-tmp))

  ### Cálculo de la constante psicometrica.
  Cp <- (0.001628*(101.3*((293-0.0065*Z)/293)^5.26)/1)

  ### Cálculo de la velocidad del viento.
  Vv <- ((4.87*uz)/log((67.8*z)-54.2))

  ### Cálculo del déficit de presión de vapor en la atmosfera.
  Dp <- (es-ea)

  ### Cálculo total de la evotranspiración.
  total <- ((0.408*Sv*Rn-Fc)+(Cp*(900/Tm+273)*Vv*Dp))/((Sv+Cp)*(1+0.344*Vv))
  return(total)
}
evotranspiración(17.27, 16.91, 17.18, 8, 1.6, 2, 16, 40, 11.5, 17.02)
