int __must_check pci_create_sysfs_dev_files (struct pci_dev *pdev)
{
	int retval;
	int rom_size = 0;
	struct bin_attribute *attr;

	if (!sysfs_initialized)
		return -EACCES;

	if (pdev->cfg_size < PCI_CFG_SPACE_EXP_SIZE)
		retval = sysfs_create_bin_file(&pdev->dev.kobj, &pci_config_attr);
	else
		retval = sysfs_create_bin_file(&pdev->dev.kobj, &pcie_config_attr);
	if (retval)
		goto err;

	retval = pci_create_resource_files(pdev);
	if (retval)
		goto err_config_file;

	if (pci_resource_len(pdev, PCI_ROM_RESOURCE))
		rom_size = pci_resource_len(pdev, PCI_ROM_RESOURCE);
	else if (pdev->resource[PCI_ROM_RESOURCE].flags & IORESOURCE_ROM_SHADOW)
		rom_size = 0x20000;

	/* If the device has a ROM, try to expose it in sysfs. */
	if (rom_size) {
		attr = kzalloc(sizeof(*attr), GFP_ATOMIC);
		if (!attr) {
			retval = -ENOMEM;
			goto err_resource_files;
		}
		sysfs_bin_attr_init(attr);
		attr->size = rom_size;
		attr->attr.name = "rom";
		attr->attr.mode = S_IRUSR;
		attr->read = pci_read_rom;
		attr->write = pci_write_rom;
		retval = sysfs_create_bin_file(&pdev->dev.kobj, attr);
		if (retval) {
			kfree(attr);
			goto err_resource_files;
		}
		pdev->rom_attr = attr;
	}

	if ((pdev->class >> 8) == PCI_CLASS_DISPLAY_VGA) {
		retval = device_create_file(&pdev->dev, &vga_attr);
		if (retval)
			goto err_rom_file;
	}

	/* add platform-specific attributes */
	retval = pcibios_add_platform_entries(pdev);
	if (retval)
		goto err_vga_file;

	/* add sysfs entries for various capabilities */
	retval = pci_create_capabilities_sysfs(pdev);
	if (retval)
		goto err_vga_file;

	return 0;

err_vga_file:
	if ((pdev->class >> 8) == PCI_CLASS_DISPLAY_VGA)
		device_remove_file(&pdev->dev, &vga_attr);
err_rom_file:
	if (rom_size) {
		sysfs_remove_bin_file(&pdev->dev.kobj, pdev->rom_attr);
		kfree(pdev->rom_attr);
		pdev->rom_attr = NULL;
	}
err_resource_files:
	pci_remove_resource_files(pdev);
err_config_file:
	if (pdev->cfg_size < PCI_CFG_SPACE_EXP_SIZE)
		sysfs_remove_bin_file(&pdev->dev.kobj, &pci_config_attr);
	else
		sysfs_remove_bin_file(&pdev->dev.kobj, &pcie_config_attr);
err:
	return retval;
}
