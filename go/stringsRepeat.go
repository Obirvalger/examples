package main

import (
  "fmt"
  "bytes"
)

func RepeatStr(repititions int, value string) string {
  var buffer bytes.Buffer
  for i := 0; i < repititions; i++ {
    buffer.WriteString(value)
  }
  return fmt.Sprint(buffer.String())
}

func main() {
  fmt.Println(RepeatStr(5, "a"))
}
