open Ast

type error_message = string
type input = string

val parse : input -> (declaration list, error_message) result
