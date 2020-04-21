# External Editor

## Description

### Dependencies
This function is only compatable with:

 Linux OS (possibly MacOS)
 gnome-terminal
 nvim
 rstudioapi must be installed

All this does is locate the current file using rstudioapi and then open use gnome-terminal -- nvim to open it, the functions used are:

### nvim
R
OpeninNvimWithGnomeTerminal <- function(){
  library(rstudioapi)
  system(paste("gnome-terminal -- nvim ", rstudioapi::getSourceEditorContext()$path))
}


### Nautilus
R
OpeninNautilus <- function(){
  library(rstudioapi)
  system(paste("nautilus ", rstudioapi::getSourceEditorContext()$path))
}


## Usage

 If you want to open the file in nvim execute:
     OpeninNvimWithGnomeTerminal() 
 If you want to open the file in nautilus execute:
     OpeninNautilus() 

## Examples

Examples would be the same as the usage.
