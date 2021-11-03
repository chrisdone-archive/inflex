In your container ports, specify names. One for normal HTTP, and one for the metrics:

```yaml
        ports:
          - containerPort: 80
            name: nginx-port
          - containerPort: 9090
            name: metrics-port    # used by prometheus 
```

Then in your service for exposing your web site, expose by name the `nginx-port` for example. Later, we'll use `metrics-port` so that prometheus will discover only port `9090` (metrics-port) with `/metrics` on it, and ignore your port `80` (nginx-port).

```yaml
apiVersion: v1
kind: Service
metadata:
  name: inflex-server
spec:
  type: ClusterIP
  ports:
  - port: 80
    targetPort: nginx-port  # use the nginx-port
  selector:
    app: inflex-server
```