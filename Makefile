BASE=../typescript/

#compiler source location
CSRC=$(BASE)src/compiler

#compiler services source location
SSRC=$(BASE)src/services

# STRC_LOCAL=$(HOST) $(BUILT_LOCAL)/tsc.js -cflowu 
# STRC_LKG=$(HOST) $(BIN)/tsc.js -cflowu 
STRC_LOCAL=tsc -cflowu

COMPILER_SOURCES_BASE= \
  $(CSRC)/diagnostics.ts \
  $(CSRC)/flags.ts \
  $(CSRC)/nodeTypes.ts \
  $(CSRC)/hashTable.ts \
  $(CSRC)/printContext.ts \
  $(CSRC)/scopeWalk.ts \
  $(CSRC)/typeCollection.ts \
  $(CSRC)/scopeAssignment.ts \
  $(CSRC)/binder.ts \
  $(CSRC)/tokens.ts \
  $(CSRC)/ast.ts \
  $(CSRC)/astWalker.ts \
  $(CSRC)/astPath.ts \
  $(CSRC)/astLogger.ts \
  $(CSRC)/scanner.ts \
  $(CSRC)/parser.ts \
  $(CSRC)/symbolScope.ts \
  $(CSRC)/types.ts \
  $(CSRC)/signatures.ts \
  $(CSRC)/symbols.ts \
  $(CSRC)/errorReporter.ts \
  $(CSRC)/typeFlow.ts \
  $(CSRC)/typeChecker.ts \
  $(CSRC)/base64.ts \
  $(CSRC)/sourceMapping.ts \
  $(CSRC)/emitter.ts \
  $(CSRC)/precompile.ts \
  $(CSRC)/incrementalParser.ts \
  $(CSRC)/pathUtils.ts \
  $(CSRC)/referenceResolution.ts \
  $(CSRC)/typescript.ts

COMPILER_SOURCES=$(COMPILER_SOURCES_BASE)
FRONTEND_SOURCES=$(COMPILER_SOURCES) $(CSRC)/io.ts $(CSRC)/optionsParser.ts $(CSRC)/tsc.ts 

SERVICES_SOURCES_BASE= \
  $(SSRC)/es5compat.ts \
  $(SSRC)/formatting/formatting.ts \
  $(SSRC)/formatting/interop.ts \
  $(SSRC)/formatting/formattingContext.ts \
  $(SSRC)/formatting/formattingManager.ts \
  $(SSRC)/formatting/formattingRequestKind.ts \
  $(SSRC)/formatting/formattingTask.ts \
  $(SSRC)/formatting/iformatter.ts \
  $(SSRC)/formatting/ilineIndentationResolver.ts \
  $(SSRC)/formatting/indentationBag.ts \
  $(SSRC)/formatting/indentationEdgeFinder.ts \
  $(SSRC)/formatting/indentationEditInfo.ts \
  $(SSRC)/formatting/indentationInfo.ts \
  $(SSRC)/formatting/indenter.ts \
  $(SSRC)/formatting/matchingBlockFinderTask.ts \
  $(SSRC)/formatting/parseNode.ts \
  $(SSRC)/formatting/parseNodeExtensions.ts \
  $(SSRC)/formatting/parseTree.ts \
  $(SSRC)/formatting/rule.ts \
  $(SSRC)/formatting/ruleAction.ts \
  $(SSRC)/formatting/ruleDescriptor.ts \
  $(SSRC)/formatting/ruleFlag.ts \
  $(SSRC)/formatting/ruleOperation.ts \
  $(SSRC)/formatting/ruleOperationContext.ts \
  $(SSRC)/formatting/rules.ts \
  $(SSRC)/formatting/rulesMap.ts \
  $(SSRC)/formatting/rulesProvider.ts \
  $(SSRC)/formatting/smartIndentManager.ts \
  $(SSRC)/formatting/smartIndentTask.ts \
  $(SSRC)/formatting/statementFinderTask.ts \
  $(SSRC)/formatting/textEditInfo.ts \
  $(SSRC)/formatting/tokenRange.ts \
  $(SSRC)/formatting/tokenSpan.ts \
  $(SSRC)/classifier.ts \
  $(SSRC)/coreServices.ts \
  $(SSRC)/scriptSyntaxAST.ts \
  $(SSRC)/compilerState.ts \
  $(SSRC)/braceMatchingManager.ts \
  $(SSRC)/symbolSet.ts \
  $(SSRC)/symbolTree.ts \
  $(SSRC)/overridesCollector.ts \
  $(SSRC)/languageService.ts \
  $(SSRC)/shims.ts \
  $(SSRC)/typescriptServices.ts

SERVICES_SOURCES= \
  $(COMPILER_SOURCES_BASE) \
  $(SERVICES_SOURCES_BASE)

all:
#	$(STRC_LOCAL) -noresolve -target es5 $(SERVICES_SOURCES) $(CSRC)/io.ts $(CSRC)/optionsParser.ts $(HSRC)/exec.ts  $(HSRC)/diff.ts $(HSRC)/harness.ts $(HSRC)/baselining.ts $(HSRC)/external/json2.ts dumper.ts -out dumper.js
	$(STRC_LOCAL) -noresolve -target es5 $(SERVICES_SOURCES) $(CSRC)/io.ts $(CSRC)/optionsParser.ts exec.ts harness.ts external/json2.ts isense.ts -out isense.js

# $(HSRC)/baselining.ts $(HSRC)/dumpAST-baselining.ts $(HSRC)/external/json2.ts $(HSRC)/runner.ts -out $(BUILT_LOCALTEST)/run.js
