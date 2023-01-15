function ToH ( n, from, to, mid )
{
	if ( n == 0 )
	{
		return;
	}
	ToH( n - 1, from, mid, to );
	console.log( "Move disk " + n + " from " + from + " to " + to );
	ToH( n - 1, mid, to, from );
}
ToH( 4, "A", "B", "C" );
