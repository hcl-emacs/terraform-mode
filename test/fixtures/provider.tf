# blah blah
terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5"
    }
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.47"
    }
  }
}
