#pragma unused symbol
#define SYMBOL_DEFINED
//#define OTHER_THING

// huh...
// completely bogus code, for now

public OnClientConnect()
{
	return "test";
}

/* test */

public Action:Event_PlayerDisconnect()
{
	/*decl String:cell[256];*/
	new String:test[index[d]], other[256], nother[MAXPLAYERS];
	
	new value, String:value = 5;
	new String:value, value;
	new value = 5, value;
	
	if (test THIS)
	{
		This;
		Is a test;
	}

	new fvalue = 20, value[256], String:test = false;
	new bool:bval = true;
	new Handle:value = INVALID_HANDLE / 5;
	value =  Cosine(value);

	special
	this;

	if (test) //comment
		dofunc();

	while (test) /* comment */
		dofunc();

	if (test) dofunc();

	for (this; that; what)
		try();

	return Plugin_Handled;
}
