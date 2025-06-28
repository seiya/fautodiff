class Vardict:
    def __init__(self):
        self._data = []

    def __setitem__(self, key, value):
        for i, (k, _) in enumerate(self._data):
            if k == key:
                self._data[i] = (key, value)
                return
        self._data.append((key, value))

    def __getitem__(self, key):
        for k, v in self._data:
            if k == key:
                return v
        raise KeyError(key)

    def __contains__(self, key):
        return any(k == key for k, _ in self._data)

    def __delitem__(self, key):
        for i, (k, _) in enumerate(self._data):
            if k == key:
                del self._data[i]
                return
        raise KeyError(key)

    def keys(self):
        return [k for k, _ in self._data]

    def values(self):
        return [v for _, v in self._data]

    def items(self):
        return list(self._data)

    def __len__(self):
        return len(self._data)

    def __iter__(self):
        for k, _ in self._data:
            yield k

    def __str__(self):
        return ", ".join([f"{k}=>{v}" for k,v in self._data])

    def remove(self, key):
        del self[key]

    def copy(self) -> "Vardict":
        obj = type(self)()
        for k, v in self._data:
            obj[k] = v
        return obj
