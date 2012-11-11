///<reference path='harness.ts'/>
///<reference path='external\json2.ts'/>
///<reference path='..\typescript\src\compiler\optionsParser.ts'/>

class Isense {

    constructor (public ioHost: IIO) { 
    }

	public dump() {
		var line: number = 0;
		var col: number = 0;

        var opts = new OptionsParser(this.ioHost);

        opts.option('line', {
            usage: 'Cursor line',
            type: 'num',
            set: (str) => {
                line = parseInt(str);
            }
        }, 'l');

        opts.option('col', {
            usage: 'Cursor column',
            type: 'num',
            set: (str) => {
                col = parseInt(str);
            }
        }, 'c');

        var printedUsage = false;
        opts.flag('help', {
            usage: 'Print this message',
            set: (type) => {
                opts.printUsage();
                printedUsage = true;
            }
        }, 'h');

        opts.parse(this.ioHost.arguments);

		var i;

		// Set up the compiler
		var typescriptLS = new Harness.TypeScriptLS();
		var refname = "";

        for (var i = 0; i < opts.unnamed.length; i++) {
            var file = opts.unnamed[i];

			var filenameIndex = file.lastIndexOf('\\');
			var filename = filenameIndex >= 0 ? file.substr(filenameIndex) : file;
			var scriptText = IO.readFile(file).trim();

			if (i == 0)
				refname = filename;

			typescriptLS.addScript(filename, scriptText, true);
		}

		// Get the language service
		var ls = typescriptLS.getLanguageService();

        var script = ls.languageService.getScriptAST(refname);
        var lineMap = script.locationInfo.lineMap;
        var pos = lineMap[line] + (col - 1);

		function parse(str: string) {
			var m = str.match(/^{'result':(.*)}$/);
			if (m == null)
				return null;
			return JSON2.parse(m[1]);
		}
        var info = {
			pos: pos,
			signature: parse(ls.getSignatureAtPosition(refname, pos)),
			member: parse(ls.getCompletionsAtPosition(refname, pos, true)),
			nomember: parse(ls.getCompletionsAtPosition(refname, pos, false)),
			type: parse(ls.getTypeAtPosition(refname, pos)),
			def: parse(ls.getDefinitionAtPosition(refname, pos)),
		};
        this.ioHost.printLine(JSON2.stringify(info).trim());
	}
}

var isense = new Isense(IO);
isense.dump();
