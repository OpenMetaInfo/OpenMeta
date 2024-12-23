package info.openmeta.framework.orm.domain.antlr;

import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.antlr.gen.FilterExprBaseVisitor;
import info.openmeta.framework.orm.domain.antlr.gen.FilterExprParser;
import info.openmeta.framework.orm.enums.LogicOperator;

import java.util.ArrayList;
import java.util.List;

public class FilterExprVisitorImpl extends FilterExprBaseVisitor<Filters> {
    @Override
    public Filters visitParenExpr(FilterExprParser.ParenExprContext ctx) {
        return visit(ctx.expr());
    }

    @Override
    public Filters visitAndExpr(FilterExprParser.AndExprContext ctx) {
        Filters left = visit(ctx.expr(0));
        Filters right = visit(ctx.expr(1));
        if (LogicOperator.AND.equals(left.getLogicOperator())) {
            left.and(right);
            return left;
        } else {
            return Filters.and(left, right);
        }
    }

    @Override
    public Filters visitOrExpr(FilterExprParser.OrExprContext ctx) {
        Filters left = visit(ctx.expr(0));
        Filters right = visit(ctx.expr(1));
        if (LogicOperator.OR.equals(left.getLogicOperator())) {
            left.or(right);
            return left;
        } else {
            return Filters.or(left, right);
        }
    }

    @Override
    public Filters visitUnitExpr(FilterExprParser.UnitExprContext ctx) {
        if (ctx.unit() instanceof FilterExprParser.FilterUnitExprContext unitContext) {
            String field = unitContext.FIELD().getText();
            Operator operator = Operator.of(unitContext.OPERATOR().getText());
            Object value = parseValue(unitContext.value());
            return Filters.of(field, operator, value);
        }
        throw new IllegalArgumentException("Unsupported unit expression: " + ctx.unit().getClass().getName());
    }

    private Object parseValue(FilterExprParser.ValueContext ctx) {
        if (ctx instanceof FilterExprParser.SingleValueExprContext singleValueCtx) {
            FilterExprParser.SingleValueContext singleValue = singleValueCtx.singleValue();
            return parseSingleValue(singleValue);
        } else if (ctx instanceof FilterExprParser.ListValueExprContext listValueCtx) {
            List<Object> list = new ArrayList<>();
            if (listValueCtx.listValue() instanceof FilterExprParser.ListValueContext valueListContext) {
                for (FilterExprParser.SingleValueContext valueCtx : valueListContext.singleValue()) {
                    list.add(parseSingleValue(valueCtx)); // Recursively parse each value in the list
                }
            }
            return list;
        }
        throw new IllegalArgumentException("Unsupported value context");
    }

    private Object parseSingleValue(FilterExprParser.SingleValueContext singleValue) {
        if (singleValue.NUMBER() != null) {
            String number = singleValue.NUMBER().getText();
            if (number.contains(".")) {
                return Double.parseDouble(number);
            } else {
                return Integer.parseInt(number);
            }
        } else if (singleValue.BOOLEAN() != null) {
            return Boolean.parseBoolean(singleValue.BOOLEAN().getText());
        } else if (singleValue.QUOTED_STRING() != null) {
            String text = singleValue.QUOTED_STRING().getText();
            return text.substring(1, text.length() - 1); // Remove the surrounding quotes
        }
        throw new IllegalArgumentException("Unsupported single value context");
    }

    private Object parseListValue(FilterExprParser.ListValueContext listValue) {
        List<Object> list = new ArrayList<>();
        for (FilterExprParser.SingleValueContext valueCtx : listValue.singleValue()) {
            list.add(parseSingleValue(valueCtx)); // Recursively parse each value in the list
        }
        return list;
    }
}
