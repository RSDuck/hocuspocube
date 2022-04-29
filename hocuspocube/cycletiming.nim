import
    heapqueue

const
    gekkoCyclesPerSecond* = 486'i64*1000*1000
    gekkoMaxSlice* = 1000'i64

    gekkoCyclesPerTbCycle* = 12

    gekkoCyclesPerDspCycle* = 486'i64 div 81'i64

    gekkoCyclesPerViCycle* = [486'i64 div 27'i64, 486'i64 div 54'i64]

    gekkoCyclesPerAiSample* = gekkoCyclesPerSecond div 48_000

type
    ScheduledEvent = object
        priority: int32 # over engineering: it's over 9000!
        timestamp: int64
        handler: proc(timestamp: int64)
        token: EventToken

    EventToken* = distinct int

proc `<`(a, b: ScheduledEvent): bool =
    if a.timestamp == b.timestamp: a.priority < b.priority else: a.timestamp < b.timestamp

proc `==`*(a, b: EventToken): bool {.borrow.}

var
    upcomingEvents: HeapQueue[ScheduledEvent]
    nextToken = 1

    gekkoTarget*: int64
    dspTimestamp*: int64

const InvalidEventToken* = EventToken 0

proc nearestEvent*(): int64 =
    if upcomingEvents.len > 0: upcomingEvents[0].timestamp else: high(int64)

proc isEventScheduled*(token: EventToken): bool =
    for i in 0..<upcomingEvents.len:
        if upcomingEvents[i].token == token:
            return true
    false

import gekko/gekko

proc gekkoTimestamp*(): int64 =
    gekkoState.negativeCycles + gekkoTarget

proc reschedule() =
    if nearestEvent() < gekkoTarget:
        let curTime = gekkoTimestamp()
        gekkoTarget = nearestEvent()
        gekkoState.negativeCycles = int32(curTime - gekkoTarget)

proc scheduleEvent*(timestamp: int64, priority: int32, handler: proc(timestamp: int64)): EventToken =
    result = EventToken nextToken
    inc nextToken
    upcomingEvents.push ScheduledEvent(timestamp: timestamp, priority: priority, handler: handler, token: result)
    reschedule()

proc cancelEvent*(token: var EventToken) =
    assert(token != InvalidEventToken)
    for i in 0..<upcomingEvents.len:
        if upcomingEvents[i].token == token:
            token = InvalidEventToken
            upcomingEvents.del(i)
            reschedule()
            return
    token = InvalidEventToken
    #assert false, "tried to cancel event which isn't scheduled"

proc processEvents*(curTime: int64) =
    while upcomingEvents.len > 0 and curTime >= upcomingEvents[0].timestamp:
        let evt = upcomingEvents.pop()
        evt.handler(evt.timestamp)
