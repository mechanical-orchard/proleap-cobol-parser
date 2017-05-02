/*
 * Copyright (C) 2016, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.asg.metamodel.procedure.perform;

import java.util.List;

import io.proleap.cobol.Cobol85Parser.PerformTypeContext;
import io.proleap.cobol.asg.metamodel.CobolDivisionElement;
import io.proleap.cobol.asg.metamodel.call.Call;

public interface PerformProcedureStatement extends CobolDivisionElement {

	void addCall(Call call);

	void addCalls(List<Call> calls);

	PerformType addPerformType(PerformTypeContext ctx);

	List<Call> getCalls();

	PerformType getPerformType();
}
