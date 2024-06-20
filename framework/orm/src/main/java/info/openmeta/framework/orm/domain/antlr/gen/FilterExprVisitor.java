package info.openmeta.framework.orm.domain.antlr.gen;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link FilterExprParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface FilterExprVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by the {@code AndExpr}
	 * labeled alternative in {@link FilterExprParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAndExpr(FilterExprParser.AndExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code UnitExpr}
	 * labeled alternative in {@link FilterExprParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnitExpr(FilterExprParser.UnitExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ParenExpr}
	 * labeled alternative in {@link FilterExprParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParenExpr(FilterExprParser.ParenExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code OrExpr}
	 * labeled alternative in {@link FilterExprParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOrExpr(FilterExprParser.OrExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code FilterUnitExpr}
	 * labeled alternative in {@link FilterExprParser#unit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFilterUnitExpr(FilterExprParser.FilterUnitExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code SingleValueExpr}
	 * labeled alternative in {@link FilterExprParser#value}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSingleValueExpr(FilterExprParser.SingleValueExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ListValueExpr}
	 * labeled alternative in {@link FilterExprParser#value}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitListValueExpr(FilterExprParser.ListValueExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link FilterExprParser#singleValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSingleValue(FilterExprParser.SingleValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link FilterExprParser#listValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitListValue(FilterExprParser.ListValueContext ctx);
}