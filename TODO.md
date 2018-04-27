
* Rather than storing allX as a field of X, I should wrap X in a tuple (x, allX) so that there is
  only structured data in X.

* I should either add a custom Read instance to Timestamp or a validator to the form so that
  values of Timestamp that are not serializeable were impossible to enter.