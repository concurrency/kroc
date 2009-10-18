/*
 *  LLVM Pass for Fixing Tail Calls (Prior to Native Code Generation)
 *  Copyright (C) 2009  Carl Ritson <cgr@kent.ac.uk>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/ADT/SmallPtrSet.h"

using namespace llvm;

namespace {
	struct TailCallFix : public FunctionPass {

		static char ID;
		TailCallFix() : FunctionPass(&ID) {}

		virtual bool runOnFunction(Function &F) {
			SmallVector<BasicBlock*, 16> fixUpBlocks;
			bool modified = false;

			for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {
				/* Do we have a tail call? */
				CallInst* callInst;
				if (!(callInst = dyn_cast<CallInst>(&*I)))
					continue;
				if (!callInst->isTailCall())
					continue;
				
				++I;

				/* Followed by a branch? Or unreachable? */
				if (BranchInst* firstBranchInst = dyn_cast<BranchInst>(&*I)) {
					/* Follow the branch (and further branches),
					 * looking for a 'ret void'.
					 */
					BranchInst* branchInst = firstBranchInst;
					bool fixUp = false;
					do {
						BasicBlock *successor 	= branchInst->getSuccessor (0);
						Instruction &first 	= successor->front();
						
						branchInst = NULL;

						if (ReturnInst *returnInst = dyn_cast<ReturnInst>(&first)) {
							if (returnInst->getNumOperands() == 0)
								fixUp = true;
						} else if ((branchInst = dyn_cast<BranchInst>(&first))) {
							if (!(branchInst->isUnconditional()))
								branchInst = NULL;
						}
					} while (branchInst);

					/* If we found ret void then fix up is required */
					if (fixUp)
						fixUpBlocks.push_back(firstBranchInst->getParent());

				} else if (UnreachableInst* unreachableInst = dyn_cast<UnreachableInst>(&*I)) {
					fixUpBlocks.push_back(unreachableInst->getParent());
				}

				--I;
			}
		
			/* Remove branches and replace with 'ret void' */
			for (SmallVector<BasicBlock*, 16>::iterator I = fixUpBlocks.begin(), E = fixUpBlocks.end(); I != E; ++I) {
				BasicBlock *BB = *I;

				BB->getInstList().pop_back();
				ReturnInst::Create(BB->getContext(), BB);
				
				modified = true;
			}

			return modified;
		}
	};
}

char TailCallFix::ID = 0;
static RegisterPass<TailCallFix> X("tailcallfix", "Tail Call Fixup Pass");

