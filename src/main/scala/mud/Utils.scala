package mud

import java.text.SimpleDateFormat

import mud._

/**
 * This file contains all sorts of utility functions used all sorts of places in the code.
 */
object Utils
{
    def check_name(name: String): Boolean = {
        name.length() >= 3 && name.length() <= 12 && !name.toCharArray().exists { !Character.isLetter(_) }
    }

    def communicate(dMob: dMobile, txt: String, range: Int)(implicit dmobile_list: List[dMobile]): Unit = {
        range match {
            case COMM_LOCAL =>  /* everyone is in the same room for now... */
                val message = s"${dMob.name} says '${txt}'.\n\r"
                val buf = s"You say '${txt}'.\n\r"
                MudSocket.text_to_mobile(dMob, buf)

                for (xMob <- dmobile_list if xMob != dMob) {
                    MudSocket.text_to_mobile(xMob, message)
                }

            case COMM_LOG =>
                val message = s"[LOG: ${txt}]\n\r"

                for (xMob <- dmobile_list if IS_ADMIN(xMob)) {
                    MudSocket.text_to_mobile(xMob, message);
                }
 
            _: Int =>
                IO.bug("Communicate: Bad Range %d.", range)
        }
    }

    def get_timestamp(): String = {
        // TODO: this used external current_time, so make this have current time passed in to be more functional
        // tm_info = localtime(&current_time)

        java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    }

    def get_date(): String = {
        java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    }

}
