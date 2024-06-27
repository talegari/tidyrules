# tidyrules 0.2.7

- Major rewrite of tidyrules
  - rulelist class introduced with many methods, mainly `predict`
  - breaking change: `tidyRules` function no longer exists!
  - Support added to `party` models

# tidyrules 0.1.5

- Maintenance release (replace package rsample with modeldata)

# tidyrules 0.1.4

- Added rules parsable in python and SQL (default: R)

# tidyrules 0.1.3

- Rules for rpart regression model ([issue](https://github.com/talegari/tidyrules/issues/8))

# tidyrules 0.1.2

- Default option to compute confidence for C5 models is now implemented with laplace correction ([issue](https://github.com/talegari/tidyrules/issues/10))
