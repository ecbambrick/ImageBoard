"use strict"

// Functions for creating a justified gallery. Each row of images in a gallery
// will fit the exact width of the gallery and each image in a row will have
// the same height.
const Gallery = {

    // Registers the given element as a gallery with the given options.
    register: (element, { maxHeight = 250, padding = 0 } = {}) => {
        const totalWidth = element.offsetWidth;
        const images     = element.getElementsByTagName("img");
        const rows       = Gallery._partition(images, maxHeight, totalWidth, padding);

        element.style.marginBottom = "-" + padding + "px";

        for (let { images, height } of rows) {
            Gallery._resizeFirstImages(images, height, padding);
            Gallery._resizeLastImage(images, height, totalWidth, padding);
        }

        // Resizing the images can cause the scroll bar to appear thus changing
        // the element's width. Resize the last image in each row to account
        // for the new scroll bar.
        const newTotalWidth = element.offsetWidth;

        if (newTotalWidth != totalWidth)
        {
            for (let { images, height } of rows) {
                Gallery._resizeLastImage(images, height, newTotalWidth, padding);
            }
        }
    },

    // Resize all images but the last in the given list to fit within the given
    // height. The last image is handled as a separate case.
    _resizeFirstImages: (images, height, padding) => {
        for (let image of images.exceptLast()) {
            const width = image.naturalWidth * height / image.naturalHeight;

            image.style.padding = `0 ${padding}px ${padding}px 0`;
            image.height        = height;
            image.width         = width + padding;
        }
    },

    // Resize the last image in the given list to to fit within the given
    // height. Due to rounding errors, the image will shrink itself to fit
    // within the total width after each other image's width is accounted for.
    _resizeLastImage: (images, height, totalWidth, padding) => {
        const totalPadding      = images.length * padding;
        const image             = images.last();
        const totalImageLengths = images.exceptLast()
                                        .map(x => x.width)
                                        .reduce((x, y) => x + y, 0);

        const scaledWidth    = image.naturalWidth * height / image.naturalHeight + padding;
        const remainingWidth = totalWidth - totalImageLengths - totalPadding;
        const width          = Math.min(scaledWidth, remainingWidth);

        image.style.padding = `0 0 ${padding}px 0`;
        image.height        = height;
        image.width         = width + padding;
    },

    // Partitions the list of images into rows based on each image's width.
    _partition: (images, maxHeight, totalWidth, padding) => {
        let rows = [];
        let i    = 0;

        while (i < images.length) {
            let rowImages = []
            let width = 0;
            let j     = i;

            while (width < totalWidth && j < images.length) {
                const img   = images[j];
                const ratio = maxHeight / img.naturalHeight;

                width += ratio * img.naturalWidth + padding;
                j     += 1;
            }

            rows.push({
                height: Math.min(maxHeight, Math.ceil(maxHeight * totalWidth / width)),
                images: images.slice(i, j),
            });

            i = j;
        }

        return rows;
    }
}
