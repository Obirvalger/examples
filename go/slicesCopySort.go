package main

import (
    "fmt"
    "sort"
)

func main() {
    s := []int{3,4,2,1}
    fmt.Println(s)
    sc := make([]int, cap(s))
    copy(sc, s)
    sort.Ints(sc)
    fmt.Println(sc)
    fmt.Println(s, len(s), cap(s))
    var a [4]int
    copy(a[:], s)
    fmt.Println(a)
}
