Dangerous unsafeties:
---------------------

* [ ] I should either add a custom Read instance to Timestamp or a validator to the form so that
  values of Timestamp that are not serializeable were impossible to enter.

* [+] I should make sure all the text fields are trimmed and padded.

* [ ] I should see to it that FixedLengthCString could not be parametrized with zero.

Future plans:
-------------

* [ ] Rather than storing allX as a field of X, I should wrap X in a tuple (x, allX) so that there
  is only structured data in X.

* [ ] Parse the whole of the save file.

* [ ] Have an editor that can focus on each part of the save file so it can be changed
  interactively.

* [ ] Have a diff that shows differences between two saves.

* [ ] Store the overlay as a type such as [(offset, description, data)] where data is an instance
  of Serialize.  This will save some effort.
