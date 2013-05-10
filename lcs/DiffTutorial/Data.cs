using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Diagnostics;
using System.Drawing;

namespace DiffCommon
{

	//-----------------------------------------------------------------------------------------
	// Results

	public class Results
	{
		public long Space { get; protected set; }

		public IList<Snake> Snakes { get; protected set; }
		public IList<V> ForwardVs { get; protected set; }
		public IList<V> ReverseVs { get; protected set; }

		public Results( long space, IList<Snake> snakes, IList<V> forwardVs, IList<V> reverseVs )
		{
			Space = space;
			Snakes = snakes;
			ForwardVs = forwardVs;
			ReverseVs = reverseVs;
		}

		public Results( long space, IList<Snake> snakes, bool forward, IList<V> Vs )
		{
			Space = space;
			Snakes = snakes;

			if ( forward ) ForwardVs = Vs; else ReverseVs = Vs;
		}
	}

	//-----------------------------------------------------------------------------------------
	// SnakePair

	class SnakePair
	{
		public int D { get; protected set; }
		public Snake Forward { get; protected set; }
		public Snake Reverse { get; protected set; }

		public SnakePair( int d, Snake forward, Snake reverse )
		{
			D = d;
			Forward = forward;
			Reverse = reverse;
		}
	}

	//-----------------------------------------------------------------------------------------
	// Snake

	public class Snake
	{
		public int XStart;
		public int YStart;
		public int ADeleted;
		public int BInserted;
		public int DiagonalLength;
		public bool IsForward = true;
		public int DELTA = 0;

		public bool IsMiddle = false;
		public int D = -1;

		public Point StartPoint { get { return new Point( XStart, YStart ); } }

		public int XMid { get { return ( IsForward ? XStart + ADeleted : XStart - ADeleted ); } }
		public int YMid { get { return ( IsForward ? YStart + BInserted : YStart - BInserted ); } }
		public Point MidPoint { get { return new Point( XMid, YMid ); } }

		public int XEnd { get { return ( IsForward ? XStart + ADeleted + DiagonalLength : XStart - ADeleted - DiagonalLength ); } }
		public int YEnd { get { return ( IsForward ? YStart + BInserted + DiagonalLength : YStart - BInserted - DiagonalLength ); } }
		public Point EndPoint { get { return new Point( XEnd, YEnd ); } }

		public override string ToString()
		{
			return "Snake " + ( IsForward ? "F" : "R" ) + ": ( " + XStart + ", " + YStart + " ) + ( " +
				( ADeleted > 0 ? "D" + ADeleted : "" ) +
				"," +
				( BInserted > 0 ? " I" + BInserted : "" ) +
				" ) + " + DiagonalLength + " -> ( " + XEnd + ", " + YEnd + " )" +
				" k=" + ( XMid - YMid );
		}

		public Snake( bool isForward, int delta )
		{
			Debug.Assert( !( isForward && delta != 0 ) );

			IsForward = isForward;

			if ( !IsForward ) DELTA = delta;
		}

		void RemoveStubs( int a0, int N, int b0, int M )
		{
			int aMin, aMax, bMin, bMax;

			if ( IsForward )
			{
				if ( XStart == a0 && YStart == b0 - 1 && BInserted == 1 )
				{
					//Debug.WriteLine( "Stub before:" + this );

					YStart = b0;
					BInserted = 0;

					//Debug.WriteLine( "Stub after:" + this );
				}

				aMin = a0; aMax = a0 + N + M;
				bMin = b0; bMax = b0 + N + M;
			}
			else
			{
				if ( XStart == a0 + N && YStart == b0 + M + 1 && BInserted == 1 )
				{
					//Debug.WriteLine( "Stub before: " + this );

					YStart = b0 + M;
					BInserted = 0;

					//Debug.WriteLine( "Stub after:" + this );
				}

				aMin = a0 - 100; aMax = a0 + N;
				bMin = b0 - 100; bMax = b0 + M;
			}

			Debug.Assert( aMin <= XStart && XStart <= aMax );
			Debug.Assert( aMin <= XMid && XMid <= aMax );
			Debug.Assert( aMin <= XEnd && XEnd <= aMax );

			Debug.Assert( bMin <= YStart && YStart <= bMax );
			Debug.Assert( bMin <= YMid && YMid <= bMax );
			Debug.Assert( bMin <= YEnd && YEnd <= bMax );

			Debug.Assert( XMid - YMid == XEnd - YEnd ); // k
		}

		public Snake( int a0, int N, int b0, int M, bool isForward, int xStart, int yStart, int aDeleted, int bInserted, int diagonal )
		{
			XStart = xStart;
			YStart = yStart;
			ADeleted = aDeleted;
			BInserted = bInserted;
			DiagonalLength = diagonal;
			IsForward = isForward;

			RemoveStubs( a0, N, b0, M );
		}

		public Snake( int a0, int N, int b0, int M, bool isForward, int xStart, int yStart, bool down, int diagonal )
		{
			XStart = xStart;
			YStart = yStart;
			ADeleted = down ? 0 : 1;
			BInserted = down ? 1 : 0;
			DiagonalLength = diagonal;
			IsForward = isForward;

			RemoveStubs( a0, N, b0, M );
		}

		//-----------------------------------------------------------------------------------------
		// Calculate char

		internal Snake( int a0, int N, int b0, int M, bool forward, int delta, V V, int k, int d, char[] pa, char[] pb )
			: this( forward, delta )
		{
			Calculate( V, k, d, pa, a0, N, pb, b0, M );
		}

		internal Snake Calculate( V V, int k, int d, char[] pa, int a0, int N, char[] pb, int b0, int M )
		{
			if ( IsForward ) return CalculateForward( V, k, d, pa, a0, N, pb, b0, M );

			return CalculateBackward( V, k, d, pa, a0, N, pb, b0, M );
		}

		Snake CalculateForward( V V, int k, int d, char[] pa, int a0, int N, char[] pb, int b0, int M )
		{
			bool down = ( k == -d || ( k != d && V[ k - 1 ] < V[ k + 1 ] ) );

			int xStart = down ? V[ k + 1 ] : V[ k - 1 ];
			int yStart = xStart - ( down ? k + 1 : k - 1 );

			int xEnd = down ? xStart : xStart + 1;
			int yEnd = xEnd - k;

			int snake = 0;
			while ( xEnd < N && yEnd < M && pa[ xEnd + a0 ] == pb[ yEnd + b0 ] ) { xEnd++; yEnd++; snake++; }

			XStart = xStart + a0;
			YStart = yStart + b0;
			ADeleted = down ? 0 : 1;
			BInserted = down ? 1 : 0;
			DiagonalLength = snake;

			RemoveStubs( a0, N, b0, M );

			return this;
		}

		Snake CalculateBackward( V V, int k, int d, char[] pa, int a0, int N, char[] pb, int b0, int M )
		{
			bool up = ( k == d + DELTA || ( k != -d + DELTA && V[ k - 1 ] < V[ k + 1 ] ) );

			int xStart = up ? V[ k - 1 ] : V[ k + 1 ];
			int yStart = xStart - ( up ? k - 1 : k + 1 );

			int xEnd = up ? xStart : xStart - 1;
			int yEnd = xEnd - k;

			int snake = 0;
			while ( xEnd > 0 && yEnd > 0 && pa[ xEnd - 1 ] == pb[ yEnd - 1 ] ) { xEnd--; yEnd--; snake++; }

			XStart = xStart;
			YStart = yStart;
			ADeleted = up ? 0 : 1;
			BInserted = up ? 1 : 0;
			DiagonalLength = snake;

			RemoveStubs( a0, N, b0, M );

			return this;
		}

		//-----------------------------------------------------------------------------------------

	}

	//-----------------------------------------------------------------------------------------
	// V

	public class V
	{
		public bool IsForward { get; private set; }
		public int N { get; private set; }
		public int M { get; private set; }

		public int _Max;
		public int _Delta;
		public int[] _Array;

		public int this[ int k ]
		{
			get
			{
				return _Array[ k - _Delta + _Max ];
			}

			set
			{
				_Array[ k - _Delta + _Max ] = value;
			}
		}

		public int Y( int k ) { return this[ k ] - k; }

		public long Memory { get { return sizeof( bool ) + IntPtr.Size + sizeof( int ) * ( 4 + _Array.Length ); } }

		V() { }

		public V( int n, int m, bool forward, bool linear )
		{
			Debug.Assert( n >= 0 && m >= 0, "V.ctor N:" + n + " M:" + m );

			IsForward = forward;
			N = n;
			M = m;

			_Max = ( linear ? ( n + m ) / 2 + 1 : n + m );
			if ( _Max <= 0 ) _Max = 1;

			_Array = new int[ 2 * _Max + 1 ];

			InitStub( n, m, _Max );
		}

		public void InitStub( int n, int m, int max )
		{
			if ( IsForward ) this[ 1 ] = 0; // stub for forward
			else
			{
				_Delta = n - m;
				this[ n - m - 1 ] = n; // stub for backward
			}

			CheckBounds( max );
		}

		[Conditional( "DEBUG" )]
		void CheckBounds( int max )
		{
			var m = String.Format(
					"max: {0:N0}, delta: {1:N0} => [ {2:N0} .. {3:N0} ] {4}",
					max, _Delta, _Delta - max, _Delta + max, ToString() );

			if ( _Max - max < 0 || _Max + max >= _Array.Length )
				Debug.Assert( false, "CheckBounds failed: " + m );
			else
			{
				//Debug.WriteLine( "CheckBounds good: " + m );
			}
		}

		//public unsafe V CreateCopy( int d, bool isForward, int delta )
		//{
		//    Debug.Assert( !( isForward && delta != 0 ) );

		//    if ( d == 0 ) d++;

		//    var o = new V();

		//    o.IsForward = isForward;
		//    o._Max = d;
		//    if ( !isForward ) o._Delta = delta;
		//    o._Array = new int[ 2 * d + 1 ];


		//    fixed ( int* po = o._Array )
		//        if ( d <= _Max )
		//        {
		//            Marshal.Copy( _Array, ( _Max - _Delta ) - ( o._Max - o._Delta ), new IntPtr( po ), o._Array.Length );
		//        }
		//        else
		//        {
		//            Debug.Assert( false );
		//            Debug.Assert( _Delta == 0 );
		//            Marshal.Copy( _Array, 0, new IntPtr( po + d - _Max ), _Array.Length );
		//        }

		//    return o;
		//}

		public V CreateCopy( int d, bool isForward, int delta )
		{
			Debug.Assert( !( isForward && delta != 0 ) );

			if ( d == 0 ) d++;

			var o = new V();

			o.IsForward = isForward;
			o._Max = d;
			if ( !isForward ) o._Delta = delta;
			o._Array = new int[ 2 * d + 1 ];

			if ( d <= _Max )
			{
				Array.Copy( _Array, ( _Max - _Delta ) - ( o._Max - o._Delta ), o._Array, 0, o._Array.Length );
			}
			else
			{
				throw new NotImplementedException( "V.CreateCopy" );
				//Debug.Assert( false );
				//Debug.Assert( _Delta == 0 );
				//Marshal.Copy( _Array, 0, new IntPtr( po + d - _Max ), _Array.Length );
			}

			return o;
		}

		public override string ToString()
		{
			return "V " + _Array.Length + " [ " + ( _Delta - _Max ) + " .. " + _Delta + " .. " + ( _Delta + _Max ) + " ]";
		}
	}

	//-----------------------------------------------------------------------------------------
}
