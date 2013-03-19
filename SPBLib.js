function _$apply(fun, args) {
    if(args.length === 0) { return fun; }

    if(typeof(fun) === "function"){
	if(args.length > 0){
	    return _$apply(fun(args[0]), args.slice(1));
	}
	else { console.log( "Too many arguments applied to function" ); console.log(args);return; }
    }

}

function _$printString(arg){
    console.log(arg);
    return true;
}

function _$append(s1, s2){ return s1.concat(s2); }

function _$show(arg) { return arg; }

function _$do(args) { console.log("Doing"); return args[args.length - 1]; }

function _$add(a, b) { return a + b; };
function _$sub(a, b) { return a - b; };
function _$mul(a, b) { return a * b; };
function _$div(a, b) { return a / b; };

function _$gt(a, b) { return a > b; };
function _$lt(a, b) { return a < b; };
function _$eq(a, b) { return a == b; };
function _$not(a) { return !a; };
