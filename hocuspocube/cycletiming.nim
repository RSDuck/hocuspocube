import
    heapqueue

const
    geckoCyclesPerSecond* = 486'i64*1000*1000
    geckoCyclesPerFrame* = geckoCyclesPerSecond div 60
    geckoMaxSlice* = 1000'i64

    geckoCyclesPerDspCycle* = 486'i64 div 81'i64

    geckoCyclesPerViCycle* = [486'i64 div 27'i64, 486'i64 div 54'i64]

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
    nextToken = 0

    geckoTimestamp* = 0'i64
    dspTimestamp* = 0'i64

const InvalidEventToken* = EventToken(-1)

proc scheduleEvent*(timestamp: int64, priority: int32, handler: proc(timestamp: int64)): EventToken =
    result = EventToken nextToken
    inc nextToken
    upcomingEvents.push ScheduledEvent(timestamp: timestamp, priority: priority, handler: handler, token: result)

proc cancelEvent*(token: var EventToken) =
    assert(token != InvalidEventToken)
    for i in 0..<upcomingEvents.len:
        if upcomingEvents[i].token == token:
            token = InvalidEventToken
            upcomingEvents.del(i)
            return
    assert false, "tried to cancel event which isn't scheduled"

proc nearestEvent*(): int64 =
    if upcomingEvents.len > 0: upcomingEvents[0].timestamp else: high(int64)

proc processEvents*() =
    while upcomingEvents.len > 0 and geckoTimestamp >= upcomingEvents[0].timestamp:
        let evt = upcomingEvents.pop()
        evt.handler(evt.timestamp)