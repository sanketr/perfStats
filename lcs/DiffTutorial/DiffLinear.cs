using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;

using DiffCommon;

namespace DiffLinear
{
	public static class Diff
	{
		//-----------------------------------------------------------------------------------------
		// public Compare

		public static Results Compare( string a, string b )
		{
			char[] aa = a.ToCharArray();
			char[] ab = b.ToCharArray();

			return Compare( aa, ab );
		}

		public static Results Compare( char[] aa, char[] ab )
		{
			Debug.WriteLine( String.Format( "\n\n*** DiffLinear A:{0:N0} B:{1:N0} A+B:{2:N0} ***", aa.Length, ab.Length, aa.Length + ab.Length ) );

			var VForward = new V( aa.Length, ab.Length, true, true );
			var VReverse = new V( aa.Length, ab.Length, false, true );

			var snakes = new List<Snake>();
			var forwardVs = new List<V>();
			var reverseVs = new List<V>();

			Compare( snakes, forwardVs, reverseVs, aa, aa.Length, ab, ab.Length, VForward, VReverse );

			return new Results( VForward.Memory + VReverse.Memory, snakes, forwardVs, reverseVs );
		}

		//-----------------------------------------------------------------------------------------
		// Compare char

		static void Compare( List<Snake> snakes,
			List<V> forwardVs, List<V> reverseVs,
			char[] pa, int N, char[] pb, int M,
			V VForward, V VReverse )
		{
			//Debug.WriteLine( String.Format( "Compare CHAR[] N: {0:N0}, M: {1:N0}", N, M ) );

			Compare( 0, snakes, forwardVs, reverseVs, pa, 0, N, pb, 0, M, VForward, VReverse );
		}

		static void Compare( int recursion, List<Snake> snakes,
			List<V> forwardVs, List<V> reverseVs,
			char[] pa, int a0, int N, char[] pb, int b0, int M,
			V VForward, V VReverse )
		{
			//Debug.WriteLine( new String( '-', recursion ) + recursion + "> Compare( " + a0 + ", " + b0 + " ) + ( " + N + ", " + M + " ) = ( " + ( a0 + N ) + ", " + ( b0 + M ) + " )" );

			if ( M == 0 && N > 0 )
			{
				var right = new Snake( a0, N, b0, M, true, a0, b0, N, 0, 0 );
				//Debug.WriteLine( "right: " + right );
				snakes.Add( right );
			}

			if ( N == 0 && M > 0 )
			{
				var down = new Snake( a0, N, b0, M, true, a0, b0, 0, M, 0 );
				//Debug.WriteLine( "down: " + down );
				snakes.Add( down );
			}

			if ( N <= 0 || M <= 0 ) return;

			SnakePair m = DiffCommon.CalcForD.MiddleSnake( pa, a0, N, pb, b0, M, VForward, VReverse, forwardVs, reverseVs );

			//Debug.WriteLine( "d:" + m.D + " " + m.Forward + " " + m.Reverse );

			if ( recursion == 0 )
			{
				if ( m.Forward != null ) { m.Forward.IsMiddle = true; }
				if ( m.Reverse != null ) { m.Reverse.IsMiddle = true; }
			}

			if ( m.D > 1 )
			{
				var xy = ( m.Forward != null ? m.Forward.StartPoint : m.Reverse.EndPoint );
				var uv = ( m.Reverse != null ? m.Reverse.StartPoint : m.Forward.EndPoint );

				Compare( recursion + 1, snakes, null, null, pa, a0, xy.X - a0, pb, b0, xy.Y - b0, VForward, VReverse );

				if ( m.Forward != null ) snakes.Add( m.Forward );
				if ( m.Reverse != null ) snakes.Add( m.Reverse );

				Compare( recursion + 1, snakes, null, null, pa, uv.X, a0 + N - uv.X, pb, uv.Y, b0 + M - uv.Y, VForward, VReverse );

			}
			else
			{
				if ( m.Forward != null )
				{
					// D0
					if ( m.Forward.XStart > a0 )
					{
						if ( m.Forward.XStart - a0 != m.Forward.YStart - b0 ) throw new ApplicationException( "Missed D0 forward" );
						snakes.Add( new Snake( a0, N, b0, M, true, a0, b0, 0, 0, m.Forward.XStart - a0 ) );
					}

					snakes.Add( m.Forward );
				}

				if ( m.Reverse != null )
				{
					snakes.Add( m.Reverse );

					// D0
					if ( m.Reverse.XStart < a0 + N )
					{
						if ( a0 + N - m.Reverse.XStart != b0 + M - m.Reverse.YStart ) throw new ApplicationException( "Missed D0 reverse" );
						snakes.Add( new Snake( a0, N, b0, M, true, m.Reverse.XStart, m.Reverse.YStart, 0, 0, a0 + N - m.Reverse.XStart ) );
					}
				}
			}
		}

		//-----------------------------------------------------------------------------------------

	}
}
