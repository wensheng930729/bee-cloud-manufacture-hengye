import React, { useEffect } from 'react';
import { reportType as REPORT_TYPE } from '@/consts/reportForm';
import { FormItemRangePicker, FormItemInputSearch } from '@/components/FormWidget';

import { SearchFormHOC, TableForParentTab } from '@/components/TableForParentTab';

import BaseComponent from '../BaseComponent';

const params = {
  name: 'huhao',
};

const MyFormSearch = props => {
  const { getFieldDecorator, islogisticsSale } = props;
  const formItemLayout = {};

  return (
    <>
      {islogisticsSale ? (
        <FormItemInputSearch
          getFieldDecorator={getFieldDecorator}
          label="客户名"
          fieldId="customerName"
          required={false}
          inputProps={{ placeholder: '' }}
          formItemLayout={formItemLayout}
        />
      ) : (
        <FormItemInputSearch
          getFieldDecorator={getFieldDecorator}
          label="供应商"
          fieldId="supplierName"
          required={false}
          inputProps={{ placeholder: '' }}
          formItemLayout={formItemLayout}
        />
      )}

      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label="产品名称"
        fieldId="productName"
        required={false}
        inputProps={{ placeholder: '' }}
        formItemLayout={formItemLayout}
      />

      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="创建日期"
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
      _czc1.push(['_trackEvent', '查看报表', '查看运输', '', '', '']);
    } catch (error) {}
  });

  const defaultTabPagesParams = {
    type: 'logistics',
    columnsParams: {
      businessType: REPORT_TYPE.logisticsSale.businessType,
      reportType: REPORT_TYPE.logisticsSale.key,
    },
  };

  return (
    <BaseComponent defaultTabPagesParams={defaultTabPagesParams}>
      <NewCom />
    </BaseComponent>
  );
};

export default Index;
