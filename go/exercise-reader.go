package main

//import "golang.org/x/tour/reader"
import (
	"fmt"
    //"golang.org/x/tour/reader"
)

type MyReader struct{}

// TODO: Add a Read([]byte) (int, error) method to MyReader.
func (r MyReader) Read(p []byte) (int, error) {
    n := 0
    for n < len(p) {
        //p = append(p, 1)
        p[n] = 'A'
        n++
    }
    return n, nil
}

func main() {
    //reader.Validate(MyReader{})
    var r MyReader
	b := make([]byte, 10)
    //fmt.Println(len(b), cap(b))
    n, err := r.Read(b)
    fmt.Println(b, n, err)
}
