# key-value-store-gen

A package for generating code that makes it easy to integrate with key-value stores through ports

## What is This?

This package takes some JSON and generates a single file that takes care of most of the work in saving and loading it to an external key-value store via ports.

The generated file contains:

- A `Store` type to put into your model and all of the necessary functions to initiate and update that `Store` safely
- Encoders and decoders for each key of the passed JSON data
- Functions that produce and consume `Json.Encode.Value` - These are meant to be sent through ports so that data can be saved, loaded, or edited in JavaScript
- Support for saving and loading arbitrary key / value pairs (with the caveat that you will need to write your own encoder / decoder for those!)

## Getting Started

To get started, check out the `/examples` folder!
