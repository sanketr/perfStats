using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using System.Threading;

using DiffCommon;

namespace DiffGreedy
{
	static class Diff
	{
		//-----------------------------------------------------------------------------------------
		// public

		public static Results Compare( string a, string b, bool forward )
		{
			char[] aa = a.ToCharArray();
			char[] ab = b.ToCharArray();

			return Compare( aa, ab, forward );
		}

		public static Results Compare( char[] aa, char[] ab, bool forward )
		{
			var V = new V( aa.Length, ab.Length, forward, false );

			var snakes = new List<Snake>();
			var vs = new List<V>();

			Compare( snakes, vs, aa, aa.Length, ab, ab.Length, V, forward );

			return new Results( V.Memory, snakes, forward, vs );
		}

		//-----------------------------------------------------------------------------------------
		// Compare

		static void Compare( List<Snake> snakes, IList<V> vs, char[] pa, int N, char[] pb, int M, V V, bool forward )
		{
			Debug.WriteLine( "\n\nDiffGreedy " + ( forward ? "FORWARD" : "REVERSE" ) + " ( " + N + ", " + M + " )" );

			Snake last = null;

			int MAX = N + M;
			int DELTA = N - M;

			int d = 0;
			for ( ; d <= MAX ; d++ )
			{
				//Debug.WriteLine( "Calculating d: " + d );

				if ( forward )
					last = DiffCommon.CalcForD.Forward( snakes, pa, N, pb, M, V, d );
				else
					last = DiffCommon.CalcForD.Reverse( snakes, pa, N, pb, M, V, d );

				vs.Add( V.CreateCopy( d, forward, ( forward ? 0 : DELTA ) ) );

				if ( last != null ) break;
			}

			if ( last == null ) throw new InvalidOperationException( "No solution was found!" );

			//Debug.WriteLine( "Solution D: " + d + " " + last );

			if ( forward ) SolveForward( snakes, vs, pa, pb, N, M );
			else SolveReverse( snakes, vs, pa, pb, N, M );
		}

		//-----------------------------------------------------------------------------------------
		// SolveForward

		static void SolveForward( List<Snake> snakes, IList<V> vs, char[] pa, char[] pb, int N, int M )
		{
			POINT p = new POINT( N, M );

			for ( int d = vs.Count - 1 ; p.X > 0 || p.Y > 0 ; d-- )
			{
				var V = vs[ d ];

				int k = p.X - p.Y;

				int xEnd = V[ k ];
				int yEnd = xEnd - k;

				if ( xEnd != p.X || yEnd != p.Y )
					throw new ApplicationException( "No solution for " +
						"d:" + d + " k:" + k + " p:( " + p.X + ", " + p.Y + " ) V:( " + xEnd + ", " + yEnd + " )" );

				Snake solution = new Snake( 0, p.X, 0, p.Y, true, 0, V, k, d, pa, pb );

				if ( solution.XEnd != p.X || solution.YEnd != p.Y )
					throw new ApplicationException( "Missed solution for " +
						"d:" + d + " k:" + k + " p:( " + p.X + ", " + p.Y + " ) V:( " + xEnd + ", " + yEnd + " )" );

				//Debug.WriteLine( "D: " + d + " " + solution );

				snakes.Add( solution );

				p.X = solution.XStart;
				p.Y = solution.YStart;
			}
		}

		//-----------------------------------------------------------------------------------------
		// SolveReverse

		static void SolveReverse( List<Snake> snakes, IList<V> vs, char[] pa, char[] pb, int N, int M )
		{
			POINT p = new POINT( 0, 0 );

			for ( int d = vs.Count - 1 ; p.X < N || p.Y < M ; d-- )
			{
				var V = vs[ d ];

				int k = p.X - p.Y;

				int xEnd = V[ k ];
				int yEnd = xEnd - k;

				if ( xEnd != p.X || yEnd != p.Y )
					throw new ApplicationException( "No solution for " +
						"d:" + d + " k:" + k + " p:( " + p.X + ", " + p.Y + " ) V:( " + xEnd + ", " + yEnd + " )" );

				Snake solution = new Snake( p.X, N - p.X, p.Y, M - p.Y, false, N - M, V, k, d, pa, pb );

				if ( solution.XEnd != p.X || solution.YEnd != p.Y )
					throw new ApplicationException( "Missed solution for " +
						"d:" + d + " k:" + k + " p:( " + p.X + ", " + p.Y + " ) V:( " + xEnd + ", " + yEnd + " )" );

				//Debug.WriteLine( "D: " + d + " " + solution );

				snakes.Add( solution );

				p.X = solution.XStart;
				p.Y = solution.YStart;
			}
		}

		//-----------------------------------------------------------------------------------------
		// POINT

		struct POINT
		{
			public int X;
			public int Y;

			public POINT( int x, int y ) { X = x; Y = y; }
		}

		//-----------------------------------------------------------------------------------------

	}
}
