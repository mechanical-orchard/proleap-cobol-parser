package io.proleap.cobol.asg;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import io.proleap.cobol.asg.metamodel.call.DataDescriptionEntryCall;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.params.impl.CobolParserParamsImpl;
import org.junit.Test;

import io.proleap.cobol.CobolTestBase;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.asg.metamodel.data.workingstorage.WorkingStorageSection;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

public class HelloWorldTest extends CobolTestBase {
	@Test
	public void test_INKCS2023() throws Exception {
		// This test is of the CICS screen COBOL file that has already been implement by our Kohls team.
		//
		// A Flow diagram has been put together of INKCS2023 here: https://www.notion.so/INASTKRM-36477f4e9c314036a95d051960b41db8
		//
		// This test is not currently working because none of the proleap cobol formatting options work on parsing it. Note
		// that the # of characters represneting the line number are less than most other files in kohls_src... we may need to strip
		// the file's line numbers and then run analyzeCode(code, params) instead...
		//
		// Mostly this test is playing around with different data structures present in the ASG.
		// Note that we are now passing in *all* of the copybookDir explicitly via constructing this CobolParserParamsImpl

		final CobolParserParams params = new CobolParserParamsImpl();
		final File copybookDir = new File("src/test/resources/io/proleap/cobol/asg/PKMO.DP.SRCLIB");
		params.setCopyBookDirectories(Arrays.asList(copybookDir));
		params.setFormat(CobolSourceFormatEnum.TANDEM);

		final File inputFile = new File("src/test/resources/io/proleap/cobol/asg/INKCS023.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, params);

		final CompilationUnit compilationUnit = program.getCompilationUnit("DPKUT041");
		final ProgramUnit programUnit = compilationUnit.getProgramUnit();
		final DataDivision dataDivision = programUnit.getDataDivision();
		final WorkingStorageSection workingStorageSection = dataDivision.getWorkingStorageSection();
	}

	@Test
	public void test_Fib() throws Exception {
		final File inputFile = new File("src/test/resources/io/proleap/cobol/asg/Fib.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.FIXED);

		final CompilationUnit compilationUnit = program.getCompilationUnit("DPKUT041");
		final ProgramUnit programUnit = compilationUnit.getProgramUnit();
		final DataDivision dataDivision = programUnit.getDataDivision();
		final WorkingStorageSection workingStorageSection = dataDivision.getWorkingStorageSection();

		// Iterate over dataDescriptionEntries and pull out interesting info
		List<DataDescriptionEntry> dataDescriptionEntries = workingStorageSection.getDataDescriptionEntries();
		for(DataDescriptionEntry entry : dataDescriptionEntries) {
			System.out.println("Data Description Entry Name: " + entry.getName());
			System.out.println("Data Description toString: " + entry.toString());
			List<DataDescriptionEntryCall> calls = entry.getCalls();

			// Print all the calls and their line numbers
			for(DataDescriptionEntryCall call : calls) {
				System.out.println("Call: " + call.getName());
				System.out.println("Line number start: " + call.getCtx().start);
				System.out.println("Line number end: " + call.getCtx().stop);
			}
		}

		ProcedureDivision procedureDivision = programUnit.getProcedureDivision();

	}

	@Test
	public void test_DPKUT041() throws Exception {
		final CobolParserParams params = new CobolParserParamsImpl();
		final File copybookDir = new File("src/test/resources/io/proleap/cobol/asg/PKMO.DP.SRCLIB");
		params.setCopyBookDirectories(Arrays.asList(copybookDir));
		params.setFormat(CobolSourceFormatEnum.FIXED);

		final File inputFile = new File("src/test/resources/io/proleap/cobol/asg/DPKUT041.cbl");
		final Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolSourceFormatEnum.FIXED);

		final CompilationUnit compilationUnit = program.getCompilationUnit("DPKUT041");
		final ProgramUnit programUnit = compilationUnit.getProgramUnit();
		final DataDivision dataDivision = programUnit.getDataDivision();
		final WorkingStorageSection workingStorageSection = dataDivision.getWorkingStorageSection();

		// Iterate over dataDescriptionEntries and pull out interesting info
		List<DataDescriptionEntry> dataDescriptionEntries = workingStorageSection.getDataDescriptionEntries();
		for(DataDescriptionEntry entry : dataDescriptionEntries) {
			System.out.println("Data Description Entry Name: " + entry.getName());
			System.out.println("Data Description toString: " + entry.toString());
			List<DataDescriptionEntryCall> calls = entry.getCalls();

			// Print all the calls and their line numbers
			for(DataDescriptionEntryCall call : calls) {
				System.out.println("Call: " + call.getName());
				System.out.println("Line number start: " + call.getCtx().start);
				System.out.println("Line number end: " + call.getCtx().stop);
			}
		}

		ProcedureDivision procedureDivision = programUnit.getProcedureDivision();
		for(Paragraph paragraph : procedureDivision.getParagraphs()) {
			paragraph.getCalls();
			for(Statement statement : paragraph.getStatements()) {
				statement.getScope();

			}

		}

	}
}