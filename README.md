# Maid - Web服务器

## 简介

### 名字由来

Server -> Servant -> Maid

Maid就是女仆的意思

### 代码原型

*Land of Lisp*的第13章

## 作者

Liutos(<mat.liutos@gmail.com>)

## TODOs

* response header
  * <del>Status-Line</del>
  * general-header
    * <del>Date</del>
    * <del>Connection</del>
  * response-header
    * <del>Server</del>
  * entity-header
    * <del>Content-Type</del>
    * <del>Content-Length</del>
* <del>dispatcher</del>
* <del>return value as response body</del>
* <del>parse the method in request</del>
* multiple clients acception
  * <del>use the multiplexer in IOlib</del>
* access log
* <del>provide a function for setting dispatch table easily</del>
* provide a macro for using dispatch table quickly and easily
* <del>encapsulate the response returnning procedure</del>
* <del>handle the 404 situation and return the correct response header</del>
  * <del>use the condition system for implementing it</del>
* handle the HANGUP condition signaled by IOlib
* run the server as daemon
* customize the server through configuration file
