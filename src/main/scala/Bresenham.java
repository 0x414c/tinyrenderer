public class Bresenham implements Drawer<BufferedImage2D> {
    @Override
    public void line (BufferedImage2D target, int x1, int y1, int x2, int y2, int color) {
        int x, y;
        int dx, dy;
        int incx, incy;
        int balance;

        if (x2 >= x1) {
            dx = x2 - x1;
            incx = 1;
        } else {
            dx = x1 - x2;
            incx = -1;
        }

        if (y2 >= y1) {
            dy = y2 - y1;
            incy = 1;
        } else {
            dy = y1 - y2;
            incy = -1;
        }

        x = x1;
        y = y1;

        if (dx >= dy) {
            dy <<= 1;
            balance = dy - dx;
            dx <<= 1;

            while (x != x2) {
                target.set (x, y, color);
                if (balance >= 0) {
                    y += incy;
                    balance -= dx;
                }
                balance += dy;
                x += incx;
            }
            target.set (x, y, color);
        } else {
            dx <<= 1;
            balance = dx - dy;
            dy <<= 1;

            while (y != y2) {
                target.set (x, y, color);
                if (balance >= 0) {
                    x += incx;
                    balance -= dy;
                }
                balance += dx;
                y += incy;
            }
            target.set (x, y, color);
        }

        return;
    }
}
