package main

import "fmt"

// fibonacci is a function that returns
// a function that returns an int.
func fibonacci() func() int {
	n, m := 0, 1
	return func() int {
		// k := n
		n, m = m, n+m
		return n
	}
}

func main() {
	f := fibonacci()
	for i := 0; i < 5; i++ {
		fmt.Println(f())
	}
}
