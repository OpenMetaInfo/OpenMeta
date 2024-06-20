package info.openmeta.framework.orm.domain.antlr.gen;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class FilterExprParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, AND=6, OR=7, FIELD=8, OPERATOR=9, 
		NUMBER=10, BOOLEAN=11, QUOTED_STRING=12, WS=13;
	public static final int
		RULE_expr = 0, RULE_unit = 1, RULE_value = 2, RULE_singleValue = 3, RULE_listValue = 4;
	private static String[] makeRuleNames() {
		return new String[] {
			"expr", "unit", "value", "singleValue", "listValue"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'('", "')'", "'['", "','", "']'", "'AND'", "'OR'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, "AND", "OR", "FIELD", "OPERATOR", 
			"NUMBER", "BOOLEAN", "QUOTED_STRING", "WS"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "FilterExpr.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public FilterExprParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ExprContext extends ParserRuleContext {
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
	 
		public ExprContext() { }
		public void copyFrom(ExprContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class AndExprContext extends ExprContext {
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode AND() { return getToken(FilterExprParser.AND, 0); }
		public AndExprContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FilterExprVisitor ) return ((FilterExprVisitor<? extends T>)visitor).visitAndExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class UnitExprContext extends ExprContext {
		public UnitContext unit() {
			return getRuleContext(UnitContext.class,0);
		}
		public UnitExprContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FilterExprVisitor ) return ((FilterExprVisitor<? extends T>)visitor).visitUnitExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class ParenExprContext extends ExprContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public ParenExprContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FilterExprVisitor ) return ((FilterExprVisitor<? extends T>)visitor).visitParenExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class OrExprContext extends ExprContext {
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode OR() { return getToken(FilterExprParser.OR, 0); }
		public OrExprContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FilterExprVisitor ) return ((FilterExprVisitor<? extends T>)visitor).visitOrExpr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExprContext expr() throws RecognitionException {
		return expr(0);
	}

	private ExprContext expr(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ExprContext _localctx = new ExprContext(_ctx, _parentState);
		ExprContext _prevctx = _localctx;
		int _startState = 0;
		enterRecursionRule(_localctx, 0, RULE_expr, _p);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(16);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__0:
				{
				_localctx = new ParenExprContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;

				setState(11);
				match(T__0);
				setState(12);
				expr(0);
				setState(13);
				match(T__1);
				}
				break;
			case FIELD:
				{
				_localctx = new UnitExprContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(15);
				unit();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(26);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(24);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
					case 1:
						{
						_localctx = new AndExprContext(new ExprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(18);
						if (!(precpred(_ctx, 4))) throw new FailedPredicateException(this, "precpred(_ctx, 4)");
						setState(19);
						match(AND);
						setState(20);
						expr(5);
						}
						break;
					case 2:
						{
						_localctx = new OrExprContext(new ExprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(21);
						if (!(precpred(_ctx, 3))) throw new FailedPredicateException(this, "precpred(_ctx, 3)");
						setState(22);
						match(OR);
						setState(23);
						expr(4);
						}
						break;
					}
					} 
				}
				setState(28);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class UnitContext extends ParserRuleContext {
		public UnitContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_unit; }
	 
		public UnitContext() { }
		public void copyFrom(UnitContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class FilterUnitExprContext extends UnitContext {
		public TerminalNode FIELD() { return getToken(FilterExprParser.FIELD, 0); }
		public TerminalNode OPERATOR() { return getToken(FilterExprParser.OPERATOR, 0); }
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public FilterUnitExprContext(UnitContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FilterExprVisitor ) return ((FilterExprVisitor<? extends T>)visitor).visitFilterUnitExpr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final UnitContext unit() throws RecognitionException {
		UnitContext _localctx = new UnitContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_unit);
		try {
			_localctx = new FilterUnitExprContext(_localctx);
			enterOuterAlt(_localctx, 1);
			{
			setState(29);
			match(FIELD);
			setState(30);
			match(OPERATOR);
			setState(31);
			value();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValueContext extends ParserRuleContext {
		public ValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_value; }
	 
		public ValueContext() { }
		public void copyFrom(ValueContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class SingleValueExprContext extends ValueContext {
		public SingleValueContext singleValue() {
			return getRuleContext(SingleValueContext.class,0);
		}
		public SingleValueExprContext(ValueContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FilterExprVisitor ) return ((FilterExprVisitor<? extends T>)visitor).visitSingleValueExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class ListValueExprContext extends ValueContext {
		public ListValueContext listValue() {
			return getRuleContext(ListValueContext.class,0);
		}
		public ListValueExprContext(ValueContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FilterExprVisitor ) return ((FilterExprVisitor<? extends T>)visitor).visitListValueExpr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValueContext value() throws RecognitionException {
		ValueContext _localctx = new ValueContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_value);
		try {
			setState(35);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case NUMBER:
			case BOOLEAN:
			case QUOTED_STRING:
				_localctx = new SingleValueExprContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(33);
				singleValue();
				}
				break;
			case T__2:
				_localctx = new ListValueExprContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(34);
				listValue();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class SingleValueContext extends ParserRuleContext {
		public TerminalNode NUMBER() { return getToken(FilterExprParser.NUMBER, 0); }
		public TerminalNode BOOLEAN() { return getToken(FilterExprParser.BOOLEAN, 0); }
		public TerminalNode QUOTED_STRING() { return getToken(FilterExprParser.QUOTED_STRING, 0); }
		public SingleValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_singleValue; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FilterExprVisitor ) return ((FilterExprVisitor<? extends T>)visitor).visitSingleValue(this);
			else return visitor.visitChildren(this);
		}
	}

	public final SingleValueContext singleValue() throws RecognitionException {
		SingleValueContext _localctx = new SingleValueContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_singleValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(37);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 7168L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ListValueContext extends ParserRuleContext {
		public List<SingleValueContext> singleValue() {
			return getRuleContexts(SingleValueContext.class);
		}
		public SingleValueContext singleValue(int i) {
			return getRuleContext(SingleValueContext.class,i);
		}
		public ListValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_listValue; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FilterExprVisitor ) return ((FilterExprVisitor<? extends T>)visitor).visitListValue(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ListValueContext listValue() throws RecognitionException {
		ListValueContext _localctx = new ListValueContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_listValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(39);
			match(T__2);
			setState(40);
			singleValue();
			setState(45);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__3) {
				{
				{
				setState(41);
				match(T__3);
				setState(42);
				singleValue();
				}
				}
				setState(47);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(48);
			match(T__4);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 0:
			return expr_sempred((ExprContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean expr_sempred(ExprContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 4);
		case 1:
			return precpred(_ctx, 3);
		}
		return true;
	}

	public static final String _serializedATN =
		"\u0004\u0001\r3\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0001"+
		"\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0003"+
		"\u0000\u0011\b\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001"+
		"\u0000\u0001\u0000\u0005\u0000\u0019\b\u0000\n\u0000\f\u0000\u001c\t\u0000"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0002\u0001\u0002"+
		"\u0003\u0002$\b\u0002\u0001\u0003\u0001\u0003\u0001\u0004\u0001\u0004"+
		"\u0001\u0004\u0001\u0004\u0005\u0004,\b\u0004\n\u0004\f\u0004/\t\u0004"+
		"\u0001\u0004\u0001\u0004\u0001\u0004\u0000\u0001\u0000\u0005\u0000\u0002"+
		"\u0004\u0006\b\u0000\u0001\u0001\u0000\n\f2\u0000\u0010\u0001\u0000\u0000"+
		"\u0000\u0002\u001d\u0001\u0000\u0000\u0000\u0004#\u0001\u0000\u0000\u0000"+
		"\u0006%\u0001\u0000\u0000\u0000\b\'\u0001\u0000\u0000\u0000\n\u000b\u0006"+
		"\u0000\uffff\uffff\u0000\u000b\f\u0005\u0001\u0000\u0000\f\r\u0003\u0000"+
		"\u0000\u0000\r\u000e\u0005\u0002\u0000\u0000\u000e\u0011\u0001\u0000\u0000"+
		"\u0000\u000f\u0011\u0003\u0002\u0001\u0000\u0010\n\u0001\u0000\u0000\u0000"+
		"\u0010\u000f\u0001\u0000\u0000\u0000\u0011\u001a\u0001\u0000\u0000\u0000"+
		"\u0012\u0013\n\u0004\u0000\u0000\u0013\u0014\u0005\u0006\u0000\u0000\u0014"+
		"\u0019\u0003\u0000\u0000\u0005\u0015\u0016\n\u0003\u0000\u0000\u0016\u0017"+
		"\u0005\u0007\u0000\u0000\u0017\u0019\u0003\u0000\u0000\u0004\u0018\u0012"+
		"\u0001\u0000\u0000\u0000\u0018\u0015\u0001\u0000\u0000\u0000\u0019\u001c"+
		"\u0001\u0000\u0000\u0000\u001a\u0018\u0001\u0000\u0000\u0000\u001a\u001b"+
		"\u0001\u0000\u0000\u0000\u001b\u0001\u0001\u0000\u0000\u0000\u001c\u001a"+
		"\u0001\u0000\u0000\u0000\u001d\u001e\u0005\b\u0000\u0000\u001e\u001f\u0005"+
		"\t\u0000\u0000\u001f \u0003\u0004\u0002\u0000 \u0003\u0001\u0000\u0000"+
		"\u0000!$\u0003\u0006\u0003\u0000\"$\u0003\b\u0004\u0000#!\u0001\u0000"+
		"\u0000\u0000#\"\u0001\u0000\u0000\u0000$\u0005\u0001\u0000\u0000\u0000"+
		"%&\u0007\u0000\u0000\u0000&\u0007\u0001\u0000\u0000\u0000\'(\u0005\u0003"+
		"\u0000\u0000(-\u0003\u0006\u0003\u0000)*\u0005\u0004\u0000\u0000*,\u0003"+
		"\u0006\u0003\u0000+)\u0001\u0000\u0000\u0000,/\u0001\u0000\u0000\u0000"+
		"-+\u0001\u0000\u0000\u0000-.\u0001\u0000\u0000\u0000.0\u0001\u0000\u0000"+
		"\u0000/-\u0001\u0000\u0000\u000001\u0005\u0005\u0000\u00001\t\u0001\u0000"+
		"\u0000\u0000\u0005\u0010\u0018\u001a#-";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}