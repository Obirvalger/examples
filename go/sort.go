package main

import (
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
)

func main() {
	a := make([]int, len(os.Args)-1)
	for i, arg := range os.Args[1:] {
		v, err := strconv.Atoi(arg)
		if err != nil {
			log.Fatal(err)
		}
		a[i] = v
	}
	sort.Ints(a)
	fmt.Println(a)

}
