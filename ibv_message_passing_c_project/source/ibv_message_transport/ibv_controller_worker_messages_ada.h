/*
 * @file ibv_controller_worker_messages_ada.h
 * @date 11 Feb 2018
 * @author Chester Gillon
 * @brief Wrapper for ibv_controller_worker_messages.h to allow generation of Ada specs by by the GNAT g++ -dump-ada-spec option.
 * @details This:
 *          a) Includes the other files required by ibv_controller_worker_messages.h
 *          b) Wraps the definitions in an extern "C" block since g++ is used to create the Ada specs
 *             to preserve the procedure argument names, while maintaining the C linkage rather than C++ mangled names.
 */

#ifndef IBV_CONTROLLER_WORKER_MESSAGES_ADA_H_
#define IBV_CONTROLLER_WORKER_MESSAGES_ADA_H_

extern "C" {

#include <stdint.h>
#include <limits.h>

#include "ibv_controller_worker_messages.h"

}

#endif /* IBV_CONTROLLER_WORKER_MESSAGES_ADA_H_ */
