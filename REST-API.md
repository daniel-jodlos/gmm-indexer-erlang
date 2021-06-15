# Rest API offered by our server

---

## Possible parameters' descriptions

| Parameter | Type | Format | Represents |
|-----------|------|--------|------------|
| from, to, of, id | string | zone_id/name | ID of a vertex |
| type, name | string |  | Some value |
| permissions | string | xx-x- | Permissions set on given edge |
| trace | string (optional) |  | Globally unique identifier of set of events about the same operation on the graph |
| successive | boolean |  | I have no idea, but Kamil has it |
| enabled | boolean |  | Stores info whether enable or disable some setting |

---

## Manipulating Vertices

### Create new vertex

<table>
    <tr>
        <th>Path</th>
        <td>/graph/vertices</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>type</li>
            <li>name</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Get all vertices

<table>
    <tr>
        <th>Path</th>
        <td>/graph/vertices/listing</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>GET</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>{"users": list(string), "groups": list(string), "spaces": list(string), "providers": list(string)}</td>
    </tr>
</table>

### Get details of vertex by ID

<table>
    <tr>
        <th>Path</th>
        <td>/graph/vertices/details</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>GET</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>id</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>{"id": string, "type": string, "name": string, "zone": string}</td>
    </tr>
</table>

### Delete vertex

<table>
    <tr>
        <th>Path</th>
        <td>/graph/vertices/delete</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>id</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Bulk vertex request

<table>
    <tr>
        <th>Path</th>
        <td>/graph/vertices/bulk</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>{"vertices": ["type1/name1", "type2/name2", ..]}</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

---

## Manipulating Edges

### Create edge

<table>
    <tr>
        <th>Path</th>
        <td>/graph/edges</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from</li>
            <li>to</li>
            <li>permissions</li>
            <li>trace: optional</li>
            <li>successive</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Set permissions

<table>
    <tr>
        <th>Path</th>
        <td>/graph/edges/permissions</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from</li>
            <li>to</li>
            <li>permissions</li>
            <li>trace: optional</li>
            <li>successive</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Delete edge

<table>
    <tr>
        <th>Path</th>
        <td>/graph/edges/delete</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from</li>
            <li>to</li>
            <li>trace: optional</li>
            <li>successive</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Bulk create-edges request

<table>
    <tr>
        <th>Path</th>
        <td>/graph/edges/bulk</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>{"sourceZone": string, "destinationZone": string, "successive": boolean, "edges": ["fromName/toName/permissions/trace", ..]}<br/>where trace can be empty string</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

---

## Basic queries about structure of the graph (edges)

### Check if edge exists

<table>
    <tr>
        <th>Path</th>
        <td>/is_adjacent</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from</li>
            <li>to</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>boolean</td>
    </tr>
</table>

### Get permissions of the edge

<table>
    <tr>
        <th>Path</th>
        <td>/permissions</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from</li>
            <li>to</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>string</td>
    </tr>
</table>

### Get children of given vertex

<table>
    <tr>
        <th>Path</th>
        <td>/list_adjacent</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>of</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>list(string)</td>
    </tr>
</table>

### Get parents of given vertex

<table>
    <tr>
        <th>Path</th>
        <td>/list_adjacent_reversed</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>of</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>list(string)</td>
    </tr>
</table>

---

## Meta Info - Server Config

### Health check to determine if server is running

<table>
    <tr>
        <th>Path</th>
        <td>/healthcheck</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>GET</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Index ready - checks if there are any waiting or currently processed events in this zone

<table>
    <tr>
        <th>Path</th>
        <td>/index_ready</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>GET</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>boolean</td>
    </tr>
</table>

### Enable/Disable indexation

<table>
    <tr>
        <th>Path</th>
        <td>/indexation</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>PUT</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>enabled</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Enable/Disable instrumentation

<table>
    <tr>
        <th>Path</th>
        <td>/instrumentation</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>PUT</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>enabled</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Check if instrumentation is enabled

<table>
    <tr>
        <th>Path</th>
        <td>/instrumentation</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>GET</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>boolean</td>
    </tr>
</table>

### Dependent Zones - check if needed zones are running and to get stats about them

<table>
    <tr>
        <th>Path</th>
        <td>/dependent_zones</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>list(string)</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>{"zones": list(string)}</td>
    </tr>
</table>

---

## Load simulation

### Run batch of operations

<table>
    <tr>
        <th>Path</th>
        <td>/simulate_load</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>{"ops": [{..}, .., {..}]}<br/>Where inner objects have fields &lt;"t" - operation type {"a", "r", "p"}, "f" - from, "to" - to, "p" - permissions, "tr" - trace&gt;</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

---

## Main Functionality: Queries about relations between any vertices in the graph

### ISO-8601 Format

In responses described in this section there is a field called "duration",
that stores time needed to execute the request.
That duration is stored as string in format given by ISO-8601 standard,
described [here](https://en.wikipedia.org/wiki/ISO_8601#Durations).

For the needs of our application we can assume that duration is smaller than 1 minute.
In that case, string looks like this:
```"PTxS"```,
where x is a floating-point number with 6 decimal places, describing number of seconds
(the integer part) and microseconds (the fractional part).

WARNING: in current state our server doesn't send times longer than 24 hours.

### Check existence of path (is 'to' effective child of 'from'?)

<table>
    <tr>
        <th>Path</th>
        <td>/naive/reaches<br/>/indexed/reaches</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from</li>
            <li>to</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>{"duration": Time-ISO-8601, "reaches": boolean}</td>
    </tr>
</table>

### Get effective permissions 'to' possesses about 'from'

<table>
    <tr>
        <th>Path</th>
        <td>/naive/effective_permissions<br/>/indexed/effective_permissions</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from</li>
            <li>to</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>{"duration": Time-ISO-8601, "effectivePermissions": string}</td>
    </tr>
</table>

### List effective children of given vertex

<table>
    <tr>
        <th>Path</th>
        <td>/naive/members<br/>/indexed/members</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>of</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>{"duration": Time-ISO-8601, "effectivePermissions": list(string)}</td>
    </tr>
</table>

---

## Event propagation

### Single event

<table>
    <tr>
        <th>Path</th>
        <td>/events</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>id; *this is id of the event, not a vertex</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>{"type": string, "trace": string, "sender": string, "originalSender": string, "effectiveVertices": ["id1", ..]}</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Bulk of events

<table>
    <tr>
        <th>Path</th>
        <td>/events/bulk</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>{"messages": [{"vn": string, "e": Event}, ..]}<br/>Event format is described in "Single event" section</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Get events stats

<table>
    <tr>
        <th>Path</th>
        <td>/events/stats</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>GET</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>{"processing": int, "processingNanos": double, "processingByType": {"user": int, ..}, "queued": int, "outbox": int, "total": long, "load1": double, "load5": double, "long15": double}</td>
    </tr>
</table>
