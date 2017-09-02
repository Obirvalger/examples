package main

import (
	"fmt"
	"log"
	"os"
	"sort"
)

type sortRunes []rune

func (s sortRunes) Less(i, j int) bool {
	return s[i] < s[j]
}

func (s sortRunes) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

func (s sortRunes) Len() int {
	return len(s)
}

func SortString(s string) string {
	r := []rune(s)
	sort.Sort(sortRunes(r))
	return string(r)
}

func isAnagram(s1, s2 string) bool {
	return SortString(s1) == SortString(s2)

}

func main() {
	if len(os.Args) != 3 {
		log.Fatal("Need exactly 2 arguments")
	}
	fmt.Println(isAnagram(os.Args[1], os.Args[2]))
}
