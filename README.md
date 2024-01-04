# noisy
noisy is a command-line tool to add noise to simulated data. 

Author: Nicola Dengo<br/>
Università degli Studi dell'Insubria<br/>
nicola.dengo at uninsubria.it<br/>

noisy was originally developed for scattering data produced by the Debussy software package. It uses the Poisson distribution to generate synthetic noise and it does not allow for negative values.

**The intended use of noisy is to make test data more similar to real data for research purposes. YOU SHALL NOT USE NOISY TO FABRICATE FRAUDOLENT DATA.**

## Installation
noisy is distributed just as source code, so you will need to compile it on your system.
Download the whole repository from GitHub or clone it using git.

### Fortran compiler
We suggest gfortran as compiler.
Get gfortran HERE or use your trusted package manager.

Example: for Ubuntu or other Linux OSs with APT.
```
sudo apt-get install gfortran
```

### Portability
The code is portable between Linux, Windows and Mac OS.
Make sure to select your OS type on the file `src/cfg_local.inc`. Open the file with a text editor (e.g. gedit, notepad, nano, ...) and pick the correct value for the variable `isystem` between 1 (Mac OS), 2 (Linux), and 3 (Windows).

### Linux
You can use provided compilation bash script `compile.sh` by command-line.
The script requires gfortran.

```
bash compile.sh
```

If you recieve the message `  Compiled.` without any error or warining, the compilation was succesfull.
By using the provided script, you will find the executable file  `noisy` on a new `bin` folder. 

### General compiling
To compile noisy manually it is generally recommended to follow a simple strategy like the following. 

```
gfortran -c noisy_modules.f90 noisy.f90
gfortran -o noisy noisy_modules.o noisy.o
```

## Use 

### Simple use 
By simply runnging the executable, noisy will run without any option. 
You will be prompt to insert the target filename(s), but no special option will be available.
We suggest to use noisy as a command-line tool instead.

### Command-line
**Basic**

`noisy input1.xye`

**Advanced** (uses options)

`noisy --scale x.xx --silent input1.xye [input2.xye input3.xye ...]`

Compatible file formats are .cal (Debussy, 3rd column), .xye (2nd column), .xy (2nd column), and .int (1st column).
By default, noisy reads and stores the file content, and just adds the noise on a specific column of values, as specificed above.
An output file will be created with the suffix `_noisy` before the file format. For example, `input1.xye` will produce `input1_noisy.xye`.
Options can be specificed using the following flags. Flags can be specified in any order. 

The scale parameter allows to tune the relative intensity of the added noise. Scale acts as a multiplicative factor applied on the provided intensities before the synthetic noise is added. The output noisy intensities are re-normalized on the the same value. The higher the scale, the lower the noise level.<br/>
**Decimal digits in the input values will be discarded, as the values will be converted to the nearest integers before applying the synthetic noise.**

*Flags*<br/><br/>
    `--help`<br/>
        Display an help topic and quit.<br/><br/>
	`--add x.xx`<br/>
        Add a constant value x.xx to the intensities before applying the noise.<br/><br/>
	`--const x.xx`<br/>
        Use a constant noise modulated around x.xx. No --scale value will be used.<br/><br/>
	`--modulate x.xx`<br/>
        Modulate the noise proportionality to variations of the intensity.<br/><br/>
	`--norm x.xx`<br/>
        Normalize max intensity to x.xx.<br/><br/>
	`--scale x.xx`<br/>
        Specify multiplicative factor to tune the noise intensity level.<br/><br/>
	`--silent`<br/>
        Make noisy less verbose.<br/><br/>
	`--source filename`<br/>
        Use the intensity values from another file for the calculation of noise.<br/><br/>
	`--ext filename`<br/>
        Use noise values from another file (1st column).<br/><br/>
	`--int-only`<br/>
        Output only the simulated intensity column.<br/><br/>
	`--noise-include`<br/>
        Add an extra column in the output file with just noise values.<br/><br/>
	`--noise-out`<br/>
        Output just the noise values in filename_justnoise.cal.<br/><br/>
		
## Notes

- Intrinsic Fortran functions are used to generate random numbers. The seed might or might not be randomized based on your compiler version.
- Poisson-type noise is formally correct only for values that are purely deriving from counting operations, with no workup.
- The use of --scale might alter the correct shape of the Poisson distribution in case of low numbers (e.g. below 100). 

 
