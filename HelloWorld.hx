using haxe.CallStack;

function main() {
	trace("Hello World!");
	trace(haxe.CallStack.callStack().toString());
}