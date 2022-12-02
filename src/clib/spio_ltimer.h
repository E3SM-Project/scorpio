#ifndef __SPIO_LTIMER_H__
#define __SPIO_LTIMER_H__

#if defined(__cplusplus)
extern "C" {
#endif

/* Start a timer */
void spio_ltimer_start(const char *timer_name);
/* Stop a timer */
void spio_ltimer_stop(const char *timer_name);
/* Get elapsed wallclock time in seconds for the timer.
 * - If the timer does not exist wallclock time of 0 is returned
 * - A started timer needs to be stopped before querying the wallclock time
 */
double spio_ltimer_get_wtime(const char *timer_name);

#if defined(__cplusplus)
}
#endif

#endif /* __SPIO_LTIMER_H__ */

