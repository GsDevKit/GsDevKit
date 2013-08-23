OBModalFilter is used to implement the 'instance/?/class' buttons in a standard class browser. In functional terms it filters the nodes of a column according to the edge of the metagraph that they correspond to. OBModalFilter displays an OBRadioButtonBar in its column's pane, with one button per edge. Only nodes from the currently selected edge are allowed in the column.

iVars:

selection	- the currently selected edge