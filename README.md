# TopoTB
A software package for calculating the electronic structure and topological properties of the tight-binding (TB) model.

## Installation
To use the TopoTB package, you must specify the complete path to load the package through `Get` or `Needs`. Alternatively, if the TopoTB package is located in a directory included in `$Path`, you can directly call it using `Get` or `Needs`. For convenience, either of the following two lines of code can quickly open a directory contained in `$Path`, place the TopoTB package in it, and call it directly using `Get` or `Needs`.

```mathematica
SystemOpen[FileNameJoin[{$UserBaseDirectory,"Applications"}]]
```

```mathematica
SystemOpen[FileNameJoin[{$BaseDirectory,"Applications"}]]
```

Use the `FindFile` function to view the installation path of the TopoTB package and check if it is installed in the directory specified in `$Path`.

```mathematica
FindFile["TopoTB`"]
```

## Load

Load TopoTB package through `Needs`

```mathematica
Needs["TopoTB`"]
```

Load TopoTB package through `Get`

``` mathematica
<<TopoTB`
```

Currently, main capabilities in the TopoTB package are

```mathematica
Names["TopoTB`BrillouinZone2D`*"]
Names["TopoTB`FirstBrillouinZone`*"]
Names["TopoTB`HamiltonianCalculation`*"]
Names["TopoTB`TopologicalNumber`*"]
Names["TopoTB`Model`*"]
Names["TopoTB`Wannier90`*"]
Names["TopoTB`VASP`*"]
Names["TopoTB`Plots`*"]
```

Get help by running the `?+function`, such as `?HSPBand`.

## Main capabilities of TopoTB

> Calculate the band structure, density of states, Fermi surface, and spin texture of the TB model

> Calculate the Berry phase, Berry curvature, Chern number, and $Z_2$ number of the TB model

> Process band data calculated by VASP

> Process the wannier90_hr.dat files obtained by Wannier90 and analyze them

> Batch testing of parameters in the TB model

## Examples

For further details, please refer to the examples in the software package and https://arxiv.org/abs/2403.08615 .