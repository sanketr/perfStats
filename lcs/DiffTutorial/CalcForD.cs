using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;

namespace DiffCommon
{
	static class CalcForD
	{

		//-----------------------------------------------------------------------------------------
		// Forward array

		public static Snake Forward( List<Snake> snakes, char[] pa, int N, char[] pb, int M, V V, int d )
		{
			int MAX = N + M;

			for ( int k = -d ; k <= d ; k += 2 )
			{
				bool down = ( k == -d || ( k != d && V[ k - 1 ] < V[ k + 1 ] ) );

				int xStart = down ? V[ k + 1 ] : V[ k - 1 ];
				int yStart = xStart - ( down ? k + 1 : k - 1 );

				int xEnd = down ? xStart : xStart + 1;
				int yEnd = xEnd - k;

				int snake = 0;
				while ( xEnd < N && yEnd < M && pa[ xEnd ] == pb[ yEnd ] ) { xEnd++; yEnd++; snake++; }

				V[ k ] = xEnd;

				//Debug.WriteLine( "  k:" + k + " V[k-1]:" + ( k - 1 >= -MAX ? V[ k - 1 ].ToString() : "X" ) + " V[k+1]:" + ( k + 1 <= MAX ? V[ k + 1 ].ToString() : "X" ) + ( down ? " down" : " right" ) + " Start:" + xStart + "," + yStart + " Gives:" + x + "," + y + " To:" + xMax + "," + yMax );

				if ( xEnd >= N && yEnd >= M )
					return new Snake( 0, N, 0, M, true, xStart, yStart, down, snake );
			}

			return null;
		}

		//-----------------------------------------------------------------------------------------
		// Reverse array

		public static Snake Reverse( List<Snake> snakes, char[] pa, int N, char[] pb, int M, V V, int d )
		{
			int MAX = N + M;
			int DELTA = N - M;

			for ( int k = -d + DELTA ; k <= d + DELTA ; k += 2 )
			{
				bool up = ( k == d + DELTA || ( k != -d + DELTA && V[ k - 1 ] < V[ k + 1 ] ) );

				int xStart = up ? V[ k - 1 ] : V[ k + 1 ];
				int yStart = xStart - ( up ? k - 1 : k + 1 );

				int xEnd = up ? xStart : xStart - 1;
				int yEnd = xEnd - k;

				int snake = 0;
				while ( xEnd > 0 && yEnd > 0 && pa[ xEnd - 1 ] == pb[ yEnd - 1 ] ) { xEnd--; yEnd--; snake++; }

				V[ k ] = xEnd;

				//Debug.WriteLine( "  k:" + k + " V[k-1]:" + ( k - 1 >= -MAX ? V[ k - 1 ].ToString() : "X" ) + " V[k+1]:" + ( k + 1 <= MAX ? V[ k + 1 ].ToString() : "X" ) + ( up ? " up" : " left" ) + " Start:" + xStart + "," + yStart + " End:" + xEnd + "," + yEnd );

				if ( xEnd <= 0 && yEnd <= 0 )
					return new Snake( 0, N, 0, M, false, xStart, yStart, up, snake );
			}

			return null;
		}

		//-----------------------------------------------------------------------------------------
		// MiddleSnake char

		public static SnakePair MiddleSnake( char[] pa, int a0, int N, char[] pb, int b0, int M, V VForward, V VReverse, IList<V> forwardVs, IList<V> reverseVs )
		{
			//int MAX = N + M;
			int MAX = ( N + M + 1 ) / 2;
			int DELTA = N - M;

			VForward.InitStub( N, M, MAX );
			VReverse.InitStub( N, M, MAX );

			bool DeltaIsEven = ( DELTA % 2 ) == 0;

			//Debug.WriteLine( "DELTA: " + DELTA + " which is " + ( DeltaIsEven ? "even => checking reverse" : "odd => checking forward" ) );

			for ( int d = 0 ; d <= MAX ; d++ )
			{
				// forward
				// checks against reverse D-1
				try
				{
					for ( int k = -d ; k <= d ; k += 2 )
					{
						bool down = ( k == -d || ( k != d && VForward[ k - 1 ] < VForward[ k + 1 ] ) );

						int xStart = down ? VForward[ k + 1 ] : VForward[ k - 1 ];
						int yStart = xStart - ( down ? k + 1 : k - 1 );

						int xEnd = down ? xStart : xStart + 1;
						int yEnd = xEnd - k;

						int snake = 0;
						while ( xEnd < N && yEnd < M && pa[ xEnd + a0 ] == pb[ yEnd + b0 ] ) { xEnd++; yEnd++; snake++; }

						VForward[ k ] = xEnd;

						// if Δ is odd and k ϵ [ Δ - ( D - 1 ), Δ + ( D - 1 ) ]
						if ( DeltaIsEven || k < DELTA - ( d - 1 ) || k > DELTA + ( d - 1 ) ) continue;

						// if the path overlaps the furthest reaching reverse ( D - 1 )-path in diagonal k
						if ( VForward[ k ] < VReverse[ k ] ) continue;

						// overlap :)
						var forward = new Snake( a0, N, b0, M, true, xStart + a0, yStart + b0, down, snake ) { D = d };
						//Debug.WriteLine( "D:"  + d + " - " + forward );
						return new SnakePair( ( 2 * d ) - 1, forward, null );
					}
				}
				finally
				{
					if ( forwardVs != null ) forwardVs.Add( VForward.CreateCopy( d, true, 0 ) );
				}

				// backward
				// checks against forward D
				try
				{
					for ( int k = -d + DELTA ; k <= d + DELTA ; k += 2 )
					{
						bool up = ( k == d + DELTA || ( k != -d + DELTA && VReverse[ k - 1 ] < VReverse[ k + 1 ] ) );

						int xStart = up ? VReverse[ k - 1 ] : VReverse[ k + 1 ];
						int yStart = xStart - ( up ? k - 1 : k + 1 );

						int xEnd = up ? xStart : xStart - 1;
						int yEnd = xEnd - k;

						int snake = 0;
						while ( xEnd > 0 && yEnd > 0 && pa[ xEnd + a0 - 1 ] == pb[ yEnd + b0 - 1 ] ) { xEnd--; yEnd--; snake++; }

						VReverse[ k ] = xEnd;

						// remember: our k is actually k + Δ

						// if Δ is even and k + Δ ϵ [ -D, D ]
						if ( !DeltaIsEven || k < -d || k > d ) continue;

						// if the path overlaps the furthest reaching forward D-path in diagonal k + Δ
						if ( VReverse[ k ] > VForward[ k ] ) continue;

						// overlap :)
						var reverse = new Snake( a0, N, b0, M, false, xStart + a0, yStart + b0, up, snake ) { D = d };
						//Debug.WriteLine( "D:" + d + " - " + reverse );
						return new SnakePair( 2 * d, null, reverse );
					}
				}
				finally
				{
					if ( reverseVs != null ) reverseVs.Add( VReverse.CreateCopy( d, false, DELTA ) );
				}
			}

			throw new ApplicationException( "No middle snake" );
		}

		//-----------------------------------------------------------------------------------------

	}
}
