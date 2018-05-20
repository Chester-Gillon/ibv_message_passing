/*
 * @file ibv_message_bw_interface_ada.h
 * @date 14 Jan 2018
 * @author Chester Gillon
 * @brief Wrapper for ibv_message_bw_interface.h to allow generation of Ada specs by by the GNAT g++ -dump-ada-spec option.
 * @details This:
 *          a) Includes the other files required by ibv_message_bw_interface.h
 *          b) Wraps the definitions in an extern "C" block since g++ is used to create the Ada specs
 *             to preserve the procedure argument names, while maintaining the C linkage rather than C++ mangled names.
 */

#ifndef IBV_MESSAGE_BW_INTERFACE_ADA_H_
#define IBV_MESSAGE_BW_INTERFACE_ADA_H_

extern "C" {

#include <stdbool.h>
#include <stdint.h>
#include <limits.h>

#include <infiniband/verbs.h>
#include <slp.h>

#include "ibv_message_bw_interface.h"

}

#endif /* IBV_MESSAGE_BW_INTERFACE_ADA_H_ */
