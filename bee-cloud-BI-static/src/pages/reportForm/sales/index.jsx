import React, { useEffect } from 'react';
import { reportType as REPORT_TYPE } from '@/consts/reportForm';

import { FormItemRangePicker, FormItemSelect, FormItemInputSearch } from '@/components/FormWidget';

import { SearchFormHOC, TableForParentTab } from '@/components/TableForParentTab';
import BaseComponent from '../BaseComponent';

const params = {
  name: 'huhao',
};

const MyFormSearch = props => {
  const { getFieldDecorator, productsOptions } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label="采购商"
        fieldId="customerName"
        required={false}
        inputProps={{ placeholder: '' }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="产品"
        fieldId="productId"
        required={false}
        selectProps={{ options: productsOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label="合同号"
        fieldId="contractNum"
        required={false}
        inputProps={{ placeholder: '按合同号搜索' }}
        formItemLayout={formItemLayout}
      />

      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="签订日期"
        fieldId="dateRange"
        required={false}
        datePickerProps={{ format: 'YYYY-MM-DD' }}
        formItemLayout={formItemLayout}
      />
    </>
  );
};

const NewIndex = SearchFormHOC(params)(MyFormSearch);

const NewCom = TableForParentTab(params)(NewIndex);

const Index = () => {
  useEffect(() => {
    try {
      _czc1.push(['_trackEvent', '查看报表', '查看销售', '', '', '']);
    } catch (error) {}
  });

  const defaultTabPagesParams = {
    type: 'sale',

    columnsParams: {
      businessType: REPORT_TYPE.sales.businessType,
      reportType: REPORT_TYPE.sales.key,
    },
  };
  return (
    <BaseComponent defaultTabPagesParams={defaultTabPagesParams} hasTab={false}>
      <NewCom />
    </BaseComponent>
  );
};

export default Index;
