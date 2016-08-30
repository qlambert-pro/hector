static int init_slots(struct controller *ctrl)
{
	struct slot *slot;
	struct hotplug_slot *hotplug_slot;
	struct hotplug_slot_info *info;
	char name[SLOT_NAME_SIZE];
	int retval = -ENOMEM;
	int i;

	for (i = 0; i < ctrl->num_slots; i++) {
		slot = kzalloc(sizeof(*slot), GFP_KERNEL);
		if (!slot)
			goto error;

		hotplug_slot = kzalloc(sizeof(*hotplug_slot), GFP_KERNEL);
		if (!hotplug_slot)
			goto error_slot;
		slot->hotplug_slot = hotplug_slot;

		info = kzalloc(sizeof(*info), GFP_KERNEL);
		if (!info)
			goto error_hpslot;
		hotplug_slot->info = info;

		slot->hp_slot = i;
		slot->ctrl = ctrl;
		slot->bus = ctrl->pci_dev->subordinate->number;
		slot->device = ctrl->slot_device_offset + i;
		slot->hpc_ops = ctrl->hpc_ops;
		slot->number = ctrl->first_slot + (ctrl->slot_num_inc * i);
		mutex_init(&slot->lock);
		INIT_DELAYED_WORK(&slot->work, shpchp_queue_pushbutton_work);

		/* register this slot with the hotplug pci core */
		hotplug_slot->private = slot;
		hotplug_slot->release = &release_slot;
		snprintf(name, SLOT_NAME_SIZE, "%d", slot->number);
		hotplug_slot->ops = &shpchp_hotplug_slot_ops;

 		ctrl_dbg(ctrl, "Registering domain:bus:dev=%04x:%02x:%02x "
 			 "hp_slot=%x sun=%x slot_device_offset=%x\n",
 			 pci_domain_nr(ctrl->pci_dev->subordinate),
 			 slot->bus, slot->device, slot->hp_slot, slot->number,
 			 ctrl->slot_device_offset);
		retval = pci_hp_register(slot->hotplug_slot,
				ctrl->pci_dev->subordinate, slot->device, name);
		if (retval) {
			ctrl_err(ctrl, "pci_hp_register failed with error %d\n",
				 retval);
			goto error_info;
		}

		get_power_status(hotplug_slot, &info->power_status);
		get_attention_status(hotplug_slot, &info->attention_status);
		get_latch_status(hotplug_slot, &info->latch_status);
		get_adapter_status(hotplug_slot, &info->adapter_status);

		list_add(&slot->slot_list, &ctrl->slot_list);
	}

	return 0;
error_info:
	kfree(info);
error_hpslot:
	kfree(hotplug_slot);
error_slot:
	kfree(slot);
error:
	return retval;
}
