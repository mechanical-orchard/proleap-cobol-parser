/*
 * Copyright (C) 2016, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.asg.metamodel.procedure.perform;

import io.proleap.cobol.Cobol85Parser.PerformInlineStatementContext;
import io.proleap.cobol.Cobol85Parser.PerformProcedureStatementContext;
import io.proleap.cobol.asg.metamodel.procedure.Statement;

/**
 * Transfers control to one or more statements. After execution returns control
 * to the next executable statement. Thus similar to a GOTO, except that after
 * execution control returns back to the next line after the perform statement.
 */
public interface PerformStatement extends Statement {

	enum Type {
		INLINE, PROCEDURE
	}

	PerformInlineStatement addPerformInlineStatement(PerformInlineStatementContext ctx);

	PerformProcedureStatement addPerformProcedureStatement(PerformProcedureStatementContext ctx);

	PerformInlineStatement getPerformInlineStatement();

	PerformProcedureStatement getPerformProcedureStatement();

	Type getType();

	void setType(Type type);

}
