(asdf:defsystem #:dynamic-wind
  :name "dynamic-wind"
  :author "Pascal Costanza"
  :version "0.61"
  :licence "
Copyright (c) 2005 - 2010 Pascal Costanza

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
"
  :depends-on (#-lispworks #:lw-compat)
  :components ((:file "dynamic-wind-packages")
               (:file "cx-threads" :depends-on ("dynamic-wind-packages"))
               (:file "cx-dynamic-environments" :depends-on ("dynamic-wind-packages"))
               (:file "cx-dynamic-variables" :depends-on ("dynamic-wind-packages" "cx-dynamic-environments" "cx-threads"))
               (:file "cx-dynascope" :depends-on ("dynamic-wind-packages" "cx-dynamic-variables"))))
