# Installation

## Method 1: Direct loading

Place the TopoTB folder in Mathematica's Applications directory:

    SystemOpen[FileNameJoin[{$UserBaseDirectory,"Applications"}]]

Then copy the TopoTB folder into the opened directory, and load:

    Needs["TopoTB`"]

## Method 2: Using Get

    <<TopoTB`

## Verify installation

    FindFile["TopoTB`"]