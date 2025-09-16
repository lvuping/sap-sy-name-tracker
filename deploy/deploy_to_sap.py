#!/usr/bin/env python3
"""
SAP ABAP SY-UNAME Tracker - Simple Deployment Script
This script generates SAPLink-compatible XML files for easy import
"""

import os
import sys
import xml.etree.ElementTree as ET
from datetime import datetime
import base64

class ABAPDeploymentGenerator:
    def __init__(self):
        self.objects = []
        self.timestamp = datetime.now().strftime('%Y%m%d%H%M%S')

    def add_abap_file(self, filepath, object_type, object_name):
        """Add ABAP source file to deployment package"""
        if os.path.exists(filepath):
            with open(filepath, 'r', encoding='utf-8') as f:
                source = f.read()
            self.objects.append({
                'type': object_type,
                'name': object_name,
                'source': source,
                'filepath': filepath
            })
            print(f"✓ Added {object_name} ({object_type})")
        else:
            print(f"✗ File not found: {filepath}")

    def generate_saplink_xml(self):
        """Generate SAPLink XML for all objects"""
        root = ET.Element('nugget', {'name': 'SYUNAME_TRACKER'})

        for obj in self.objects:
            if obj['type'] == 'PROG':
                self._add_program(root, obj)
            elif obj['type'] == 'CLAS':
                self._add_class(root, obj)
            elif obj['type'] == 'INTF':
                self._add_interface(root, obj)

        return ET.tostring(root, encoding='unicode')

    def _add_program(self, root, obj):
        """Add program to SAPLink XML"""
        prog = ET.SubElement(root, 'PROG', {'NAME': obj['name']})
        ET.SubElement(prog, 'SOURCE').text = base64.b64encode(
            obj['source'].encode('utf-8')).decode('ascii')

    def _add_class(self, root, obj):
        """Add class to SAPLink XML"""
        clas = ET.SubElement(root, 'CLAS', {'NAME': obj['name']})
        ET.SubElement(clas, 'SOURCE').text = base64.b64encode(
            obj['source'].encode('utf-8')).decode('ascii')

    def _add_interface(self, root, obj):
        """Add interface to SAPLink XML"""
        intf = ET.SubElement(root, 'INTF', {'NAME': obj['name']})
        ET.SubElement(intf, 'SOURCE').text = base64.b64encode(
            obj['source'].encode('utf-8')).decode('ascii')

    def save_deployment_package(self, output_dir='deploy/output'):
        """Save deployment package"""
        os.makedirs(output_dir, exist_ok=True)

        # Generate SAPLink XML
        xml_content = self.generate_saplink_xml()
        xml_file = os.path.join(output_dir, f'syuname_tracker_{self.timestamp}.xml')
        with open(xml_file, 'w', encoding='utf-8') as f:
            f.write(xml_content)
        print(f"\n✓ SAPLink XML saved to: {xml_file}")

        # Generate upload script
        script_file = os.path.join(output_dir, f'upload_script_{self.timestamp}.txt')
        with open(script_file, 'w', encoding='utf-8') as f:
            f.write(self.generate_upload_script())
        print(f"✓ Upload script saved to: {script_file}")

        return xml_file, script_file

    def generate_upload_script(self):
        """Generate ABAP upload script"""
        script = f"""*&---------------------------------------------------------------------*
*& Upload Script for SY-UNAME Tracker
*& Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
*&---------------------------------------------------------------------*
*& Steps to deploy:
*& 1. Create package Z_SYUNAME_TRACKER in SE80
*& 2. Use SAPLink (Transaction ZSAPLINK) to import the XML file
*& 3. Or manually create each object and paste the source code
*&---------------------------------------------------------------------*

* Package Creation
SE80 -> Create Package: Z_SYUNAME_TRACKER

* Object Creation Order:
"""
        for obj in self.objects:
            script += f"* - {obj['type']} {obj['name']}\n"

        return script

def main():
    print("=" * 60)
    print("SAP ABAP SY-UNAME Tracker - Deployment Generator")
    print("=" * 60)

    deployer = ABAPDeploymentGenerator()

    # Add all interfaces
    interfaces = [
        ('src/abap/interfaces/zif_syuname_scanner.abap', 'ZIF_SYUNAME_SCANNER'),
        ('src/abap/interfaces/zif_syuname_taint.abap', 'ZIF_SYUNAME_TAINT'),
        ('src/abap/interfaces/zif_syuname_parser.abap', 'ZIF_SYUNAME_PARSER'),
        ('src/abap/interfaces/zif_syuname_report.abap', 'ZIF_SYUNAME_REPORT'),
    ]

    print("\nAdding Interfaces...")
    for filepath, name in interfaces:
        deployer.add_abap_file(filepath, 'INTF', name)

    # Add exception class
    print("\nAdding Exception Classes...")
    deployer.add_abap_file('src/abap/exceptions/zcx_syuname_error.abap',
                           'CLAS', 'ZCX_SYUNAME_ERROR')

    # Add implementation classes
    classes = [
        ('src/abap/classes/zcl_syuname_scanner.abap', 'ZCL_SYUNAME_SCANNER'),
        ('src/abap/classes/zcl_syuname_taint.abap', 'ZCL_SYUNAME_TAINT'),
        ('src/abap/classes/zcl_syuname_parser.abap', 'ZCL_SYUNAME_PARSER'),
        ('src/abap/classes/zcl_syuname_report.abap', 'ZCL_SYUNAME_REPORT'),
        ('src/abap/classes/zcl_syuname_factory.abap', 'ZCL_SYUNAME_FACTORY'),
        ('src/zcl_syuname_scope_manager.abap', 'ZCL_SYUNAME_SCOPE_MANAGER'),
        ('src/zcl_syuname_fieldsymbol_track.abap', 'ZCL_SYUNAME_FIELDSYMBOL_TRACK'),
        ('src/zcl_syuname_enhanced_parser.abap', 'ZCL_SYUNAME_ENHANCED_PARSER'),
        ('src/zcl_syuname_report_enhanced.abap', 'ZCL_SYUNAME_REPORT_ENHANCED'),
        ('src/zcl_syuname_move_corr_handler.abap', 'ZCL_SYUNAME_MOVE_CORR_HANDLER'),
        ('src/zcl_syuname_performance_opt.abap', 'ZCL_SYUNAME_PERFORMANCE_OPT'),
    ]

    print("\nAdding Implementation Classes...")
    for filepath, name in classes:
        deployer.add_abap_file(filepath, 'CLAS', name)

    # Add programs
    programs = [
        ('src/abap/programs/zsyuname_analyzer.abap', 'ZSYUNAME_ANALYZER'),
        ('ZSYUNAME_ANLZ.abap', 'ZSYUNAME_ANLZ'),
    ]

    print("\nAdding Programs...")
    for filepath, name in programs:
        deployer.add_abap_file(filepath, 'PROG', name)

    # Add test programs
    print("\nAdding Test Programs...")
    deployer.add_abap_file('src/abap/tests/zsyuname_test_integration.abap',
                          'PROG', 'ZSYUNAME_TEST_INTEGRATION')
    deployer.add_abap_file('src/zcl_syuname_test_validation.abap',
                          'CLAS', 'ZCL_SYUNAME_TEST_VALIDATION')

    # Save deployment package
    print("\nGenerating Deployment Package...")
    xml_file, script_file = deployer.save_deployment_package()

    print("\n" + "=" * 60)
    print("✓ Deployment package generated successfully!")
    print("=" * 60)
    print("\nNext steps:")
    print("1. Copy the XML file to your SAP system")
    print("2. Use SAPLink to import the XML file")
    print("3. Or follow the manual upload script")
    print("\nFiles generated:")
    print(f"  - XML: {xml_file}")
    print(f"  - Script: {script_file}")

if __name__ == "__main__":
    main()