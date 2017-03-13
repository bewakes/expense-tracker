# -*- coding: utf-8 -*-
# Generated by Django 1.10.5 on 2017-03-13 04:18
from __future__ import unicode_literals

from django.conf import settings
import django.contrib.auth.models
from django.db import migrations, models
import django.db.models.deletion
import django.utils.timezone


class Migration(migrations.Migration):

    initial = True

    dependencies = [
        ('auth', '0008_alter_user_username_max_length'),
    ]

    operations = [
        migrations.CreateModel(
            name='AppUser',
            fields=[
                ('user_ptr', models.OneToOneField(auto_created=True, on_delete=django.db.models.deletion.CASCADE, parent_link=True, primary_key=True, serialize=False, to=settings.AUTH_USER_MODEL)),
                ('address', models.CharField(blank=True, max_length=100, null=True)),
                ('occupation', models.CharField(blank=True, max_length=100, null=True)),
                ('has_setup', models.BooleanField(default=False)),
            ],
            options={
                'verbose_name': 'user',
                'verbose_name_plural': 'users',
                'abstract': False,
            },
            bases=('auth.user',),
            managers=[
                ('objects', django.contrib.auth.models.UserManager()),
            ],
        ),
        migrations.CreateModel(
            name='Category',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=50)),
                ('uses', models.IntegerField(default=0)),
                ('is_deleted', models.BooleanField(default=False)),
                ('description', models.CharField(blank=True, max_length=1000)),
            ],
        ),
        migrations.CreateModel(
            name='Expense',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('items', models.TextField()),
                ('description', models.CharField(blank=True, max_length=1000)),
                ('cost', models.IntegerField(default=0)),
                ('date', models.DateTimeField(default=django.utils.timezone.now)),
                ('is_deleted', models.BooleanField(default=False)),
                ('category', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to='expenses.Category')),
            ],
        ),
        migrations.CreateModel(
            name='Item',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=50)),
                ('uses', models.IntegerField(default=0)),
                ('description', models.CharField(blank=True, max_length=1000)),
                ('is_deleted', models.BooleanField(default=False)),
                ('category', models.ForeignKey(null=True, on_delete=django.db.models.deletion.CASCADE, to='expenses.Category')),
            ],
        ),
        migrations.CreateModel(
            name='ItemExpense',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('quantity', models.FloatField(default=1)),
                ('cost', models.IntegerField(default=0)),
                ('expense', models.ForeignKey(null=True, on_delete=django.db.models.deletion.CASCADE, to='expenses.Expense')),
                ('item', models.ForeignKey(null=True, on_delete=django.db.models.deletion.CASCADE, to='expenses.Item')),
            ],
        ),
        migrations.CreateModel(
            name='Organization',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=100)),
                ('is_individual', models.BooleanField(default=True)),
                ('owner', models.OneToOneField(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, to='expenses.AppUser')),
            ],
        ),
        migrations.AddField(
            model_name='item',
            name='organization',
            field=models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='items', to='expenses.Organization'),
        ),
        migrations.AddField(
            model_name='expense',
            name='item',
            field=models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to='expenses.Item'),
        ),
        migrations.AddField(
            model_name='category',
            name='organization',
            field=models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='categories', to='expenses.Organization'),
        ),
        migrations.AddField(
            model_name='appuser',
            name='organizations',
            field=models.ManyToManyField(null=True, related_name='users', to='expenses.Organization'),
        ),
    ]
