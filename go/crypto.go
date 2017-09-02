package main

import (
	"crypto/md5"
	"crypto/sha1"
	"crypto/sha256"
	"crypto/sha512"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

func main() {
	var hashesPtr = flag.String("hash", "sha256", "Choose hash sum algotithm")
	flag.Parse()
	hashes := strings.Split(*hashesPtr, ",")

	data, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		log.Fatal()
	}

	fmt.Printf("Contents: %s", data)
	for _, hash := range hashes {
		l := 7
		fmt.Printf("Sum %-[1]*s %x\n", l, hash+":", hashSum(data, hash))
	}
}

func hashSum(data []byte, hash string) []byte {
	switch hash {
	case "md5":
		res := md5.Sum(data)
		return res[:]
	case "sha1":
		res := sha1.Sum(data)
		return res[:]
	case "sha256":
		res := sha256.Sum256(data)
		return res[:]
	case "sha512":
		res := sha512.Sum512(data)
		return res[:]
	}

	return []byte{}
}
