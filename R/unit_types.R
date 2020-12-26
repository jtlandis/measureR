

setClass("unit_type")
weight <- setClass("weight", contains = "unit_type")
distance <- setClass("distance", contains = "unit_type")
time <- setClass("time", contains = "unit_type")
temperature <- setClass("temperature", contains = "unit_type")
